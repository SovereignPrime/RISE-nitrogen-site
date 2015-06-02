var init_upload = function(id, pathid) {
    $(id).on({
        dragover : function(e) {
            var dataTransfer = e.dataTransfer = e.originalEvent.dataTransfer;
            if (dataTransfer) {
                dataTransfer.dropEffect = 'copy';
            }
            e.preventDefault();
        },

        drop : function(e) {
            e.preventDefault();
            return false;
        }
    });

    upload = function(path) {
        $(obj(pathid)).val(path);
        $(obj(pathid)).trigger('change');

    };
};

function download(pathid, path) {
        $(obj(pathid)).val(path);
        $(obj(pathid)).trigger('change');
    };

