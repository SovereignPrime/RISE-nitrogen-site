%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(update_preview, {?ELEMENT_BASE(element_update_preview),
                         icon,
                         from,
                         age,
                         subject,
                         flag=true,
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

-record(vcard, {?ELEMENT_BASE(element_vcard),
                photo="undefined.png",
                name,
                email,
                phone,
                address
    }).

-record(taskrow, {?ELEMENT_BASE(element_taskrow),
                  type,
                  name,
                  due
    }).
-record(involved, {?ELEMENT_BASE(element_involved)}).
-record(addable_row, {?ELEMENT_BASE(element_addable_row),
                      num=0,
                      body,
                      options
    }).
-record(task_leaf, {?ELEMENT_BASE(element_task_leaf),
                    tid,
                    name,
                    delegate,
                    checked=false,
                    current=false,
                    due
    }).
-record(file_row, {?ELEMENT_BASE(element_file_row),
                   name,
                   type,
                   size,
                   for,
                   date,
                   linked,
                   seed,
                   peer,
                   status
    }).
-record(payment_row, {?ELEMENT_BASE(element_payment_row),
                      from,
                      to,
                      tasks,
                      due,
                      status,
                      amount,
                      currency,
                      type
                     }).
