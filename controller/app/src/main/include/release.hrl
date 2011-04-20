
-record(edist_release_elem, {elem_id, criteria, block_size, total_size, sha}).
-record(edist_release_vsn, {vsn, state=initializing, ref_count=0, elements=[]}).
-record(edist_release, {name, config, versions=dict:new()}).
-record(edist_release_block, {id, name, vsn, elem_id, row, size, data}).
