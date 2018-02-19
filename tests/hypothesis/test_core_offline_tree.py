import os
import attr
import pytest
from hypothesis import strategies as st, note
from hypothesis.stateful import Bundle, rule

from tests.common import connect_core, core_factory
from tests.hypothesis.conftest import skip_on_broken_stream


@attr.s
class OracleFS:
    root = attr.ib(default=attr.Factory(dict))

    def create_file(self, parent_path, name):
        parent_folder = self.get_folder(parent_path)
        if parent_folder is None or name in parent_folder:
            return 'invalid_path'
        parent_folder[name] = '<file>'
        return 'ok'

    def create_folder(self, parent_path, name):
        parent_folder = self.get_folder(parent_path)
        if parent_folder is None or name in parent_folder:
            return 'invalid_path'
        parent_folder[name] = {}
        return 'ok'

    def delete(self, path):
        parent_path, name = path.rsplit('/', 1)
        parent_dir = self.get_path(parent_path)
        if isinstance(parent_dir, dict) and name in parent_dir:
            del parent_dir[name]
            return 'ok'
        else:
            return 'invalid_path'

    def move(self, src, dst):
        parent_src, name_src = src.rsplit('/', 1)
        parent_dst, name_dst = dst.rsplit('/', 1)

        parent_dir_src = self.get_folder(parent_src)
        parent_dir_dst = self.get_folder(parent_dst)

        if parent_dir_src is None or name_src not in parent_dir_src:
            return 'invalid_path'
        if parent_dir_dst is None or name_dst in parent_dir_dst:
            return 'invalid_path'

        parent_dir_dst[name_dst] = parent_dir_src.pop(name_src)
        return 'ok'

    def get_folder(self, path):
        elem = self.get_path(path)
        return elem if elem != '<file>' else None
        
    def get_file(self, path):
        elem = self.get_path(path)
        return elem if elem == '<file>' else None
        
    def get_path(self, path):
        current_folder = self.root
        try:
            for item in path.split('/'):
                if item:
                    current_folder = current_folder[item]
        except KeyError:
            return None
        return current_folder


@pytest.mark.slow
@pytest.mark.trio
async def test_offline_core_tree(
    TrioDriverRuleBasedStateMachine,
    mocked_local_storage_connection,
    backend_addr,
    tmpdir,
    alice
):

    st_entry_name = st.text(min_size=1).filter(lambda x: '/' not in x)

    class CoreOfflineRWFile(TrioDriverRuleBasedStateMachine):
        Files = Bundle('file')
        Folders = Bundle('folder')
        count = 0

        async def trio_runner(self, task_status):
            mocked_local_storage_connection.reset()
            self.oracle_fs = OracleFS()

            type(self).count += 1
            config = {
                'base_settings_path': tmpdir.mkdir('try-%s' % self.count).strpath,
                'backend_addr': backend_addr,
            }

            async with core_factory(**config) as core:
                await core.login(alice)
                async with connect_core(core) as sock:

                    self.core_cmd = self.communicator.send
                    task_status.started()

                    while True:
                        msg = await self.communicator.trio_recv()
                        await sock.send(msg)
                        rep = await sock.recv()
                        await self.communicator.trio_respond(rep)

        @rule(target=Folders)
        def init_root(self):
            return '/'

        @rule(target=Files, parent=Folders, name=st_entry_name)
        @skip_on_broken_stream
        def create_file(self, parent, name):
            path = os.path.join(parent, name)
            rep = self.core_cmd({'cmd': 'file_create', 'path': path})
            note(rep)
            expected_status = self.oracle_fs.create_file(parent, name)
            assert rep['status'] == expected_status
            return path

        @rule(target=Folders, parent=Folders, name=st_entry_name)
        @skip_on_broken_stream
        def create_folder(self, parent, name):
            path = os.path.join(parent, name)
            rep = self.core_cmd({'cmd': 'folder_create', 'path': path})
            note(rep)
            expected_status = self.oracle_fs.create_folder(parent, name)
            assert rep['status'] == expected_status
            return path

        @rule(path=Files)
        @skip_on_broken_stream
        def delete_file(self, path):
            rep = self.core_cmd({'cmd': 'delete', 'path': path})
            note(rep)
            expected_status = self.oracle_fs.delete(path)
            assert rep['status'] == expected_status

        @rule(path=Folders)
        @skip_on_broken_stream
        def delete_folder(self, path):
            rep = self.core_cmd({'cmd': 'delete', 'path': path})
            note(rep)
            expected_status = self.oracle_fs.delete(path)
            assert rep['status'] == expected_status

        @rule(target=Files, src=Files, dst_parent=Folders, dst_name=st_entry_name)
        @skip_on_broken_stream
        def move_file(self, src, dst_parent, dst_name):
            dst = os.path.join(dst_parent, dst_name)
            rep = self.core_cmd({'cmd': 'move', 'src': src, 'dst': dst})
            note(rep)
            expected_status = self.oracle_fs.move(src, dst)
            assert rep['status'] == expected_status
            return dst

        @rule(target=Folders, src=Folders, dst_parent=Folders, dst_name=st_entry_name)
        @skip_on_broken_stream
        def move_folder(self, src, dst_parent, dst_name):
            dst = os.path.join(dst_parent, dst_name)
            rep = self.core_cmd({'cmd': 'move', 'src': src, 'dst': dst})
            note(rep)
            expected_status = self.oracle_fs.move(src, dst)
            assert rep['status'] == expected_status
            return dst

    await CoreOfflineRWFile.run_test()
