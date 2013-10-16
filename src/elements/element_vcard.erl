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
render_element(#vcard{id=Id, photo=Photo, name=Name, email=Email, phone=Phone, address=Address, groups=Groups}) ->
    #panel{id=Id, class="row-fluid", body=[
            #panel{class="span2", body=[
                    
                    #image{image="photo/" ++ Photo, class="image-polaroid"}
                    ]},
            #panel{class="span9", body=[
                    #panel{class="row-fluid", body=[
                            #h1{class="", body=
                                #inplace_textbox{text=Name, tag={name, Id}}
                                                 },% " <i class='clearfix icon-edit icon-large'></i>",
                            #panel{body= [ "e-mail: ",
                                        #inplace_textbox{class="inline", tag={ email, Id}, text=  Email }
                                         ]},
                            #panel{body= [
                                    "tel.: ",
                                 #inplace_textbox{class="inline", text=Phone, tag={phone, Id}}
                                    ]},
                            #panel{body= [
                                    #inplace_textbox{text=  Address, tag={address, Id}}
                                    ]},
                            #panel{body= [ [#span{class="label", text=G}, " "] || G <- Groups]}
                            ]}

                    ]},
            #panel{class="span1", body="<i class='icon-envelope icon-large'></i><br><i class='icon-group icon-large'></i><br><i class='icon-reorder icon-large'></i>"}
                    
            ]}.
