%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(update_preview, {?ELEMENT_BASE(element_update_preview),
                         uid,
                         icon,
                         from,
                         to,
                         age,
                         subject,
                         flag=true,
                         text,
						 status,
                         archive
    }).
-record(update_element, {?ELEMENT_BASE(element_update),
                         uid,
                         from,
                         to,
                         text,
                         age,
                         attachments,
                         subject,
                         collapse=true,
                         status,
                         enc=2
                        }).
-record(attachment, {?ELEMENT_BASE(element_attachment),
                     fid,
                     filename,
                     size,
                     time,
                     status
    }).

-record(vcard, {?ELEMENT_BASE(element_vcard),
                photo="undefined.png",
                name,
                email,
                phone,
                groups=[],
                address
    }).

-record(taskrow, {?ELEMENT_BASE(element_taskrow),
                  type,
                  name,
                  due
    }).
-record(involved, {?ELEMENT_BASE(element_involved),
                   person="",
                   role
                  }).
-record(to, {?ELEMENT_BASE(element_to),
            text}).
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
                   fid,
                   status
    }).
-record(payment_row, {?ELEMENT_BASE(element_payment_row),
                      pid,
                      from,
                      to,
                      tasks,
                      due,
                      status,
                      amount,
                      currency,
                      type
                     }).
-record(contact_li, {?ELEMENT_BASE(element_contact_li),
                     uid,
                     name,
                     checked
    }).
-record(group_item, {?ELEMENT_BASE(element_group_item),
                     gid,
                     name,
                     sub,
                     archive=false
    }).
-record(progressbar, {?ELEMENT_BASE(element_progressbar),
                      border_color="#000",
                      color="#000",
                      progress=0,
                      width=100,
                      bg="#fff"
    }).

-record(expander, {?ELEMENT_BASE(element_expander),
                   start=open,
                   target
    }).

-record(popup, {?ELEMENT_BASE(element_popup),
                header,
                body
    }).
