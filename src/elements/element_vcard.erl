%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_vcard).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, vcard).

-spec render_element(#vcard{}) -> body().
render_element(#vcard{photo=Photo, name=Name, email=Email, phone=Phone, address=Address}) ->
    #panel{class="row-fluid", body=[
            #panel{class="span2", body=[
                    #image{image="photo/" ++ Photo, class="image-polaroid"}
                    ]},
            #panel{class="span9", body=[
                    #panel{class="row-fluid", body=[
                            #h1{class="pull-left", text=Name}, " <i class='clearfix icon-edit icon-large'></i>",
                            #panel{body= "e-mail: " ++ Email},
                            #panel{body= "tel.: " ++ Phone},
                            #panel{body=  Address}
                            ]}

                    ]},
            #panel{class="span1", body="<i class='icon-envelope icon-large'></i><br><i class='icon-group icon-large'></i><br><i class='icon-reorder icon-large'></i>"}
                    
            ]}.
