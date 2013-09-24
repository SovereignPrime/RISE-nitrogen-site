%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(update_preview, {?ELEMENT_BASE(element_update_preview),
                         icon,
                         from,
                         age,
                         subject,
                         text
    }).
-record(update_element, {?ELEMENT_BASE(element_update),
                  from,
                  text,
                  age,
                  attachments,
                  collapse=true
    }).
-record(attachment, {?ELEMENT_BASE(element_attachment),
                     filename,
                     size,
                     time
    }).
