
-record(edist_release_refs, {vsn, block_size, total_size, ref_count}).
-record(edist_release, {name, versions=[]}).
-record(edist_release_data, {name, vsn, block_id, block_size, data}).
