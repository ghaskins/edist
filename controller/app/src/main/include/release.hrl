
-record(edist_release_vsn, {vsn, block_size, total_size, ref_count}).
-record(edist_release, {name, versions=dict:new()}).
-record(edist_release_block, {id, name, vsn, row, size, data}).
