[@@@warning "-48"]
open GdkKeysyms

let show ~view () =
  try begin
    let w = GWindow.window
        ~title: "Go to..."
        ~resizable:false
        ~type_hint:`DIALOG
        ~position:`CENTER_ALWAYS
        ~modal:true () in
    Gmisclib.Window.GeometryMemo.add (!Otherwidgets_config.geometry_memo()) ~key:"dialog-goto-line" ~window:w;
    let vbox = GPack.vbox ~border_width:8 ~spacing:8 ~packing:w#add () in
    let eb = GPack.hbox ~spacing:3 ~packing:vbox#add () in
    let _ = GMisc.label ~text:"Line Number: " ~xalign:0.0 ~width:120 ~packing:(eb#pack ~expand:false) () in
    let line = GEdit.entry ~packing:eb#add () in
    let eb = GPack.hbox ~spacing:3 ~packing:vbox#add () in
    let _ = GMisc.label ~text:"Line(Buffer) Offset: " ~xalign:0.0 ~width:120 ~packing:(eb#pack ~expand:false) () in
    let char = GEdit.entry ~packing:eb#add () in
    let _ = GMisc.separator `HORIZONTAL ~packing:vbox#add () in
    let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#add () in
    let button_ok = GButton.button ~stock:`OK ~packing:bbox#add () in
    let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#add () in
    let callback () =
      try
        let buf = view#buffer in
        let char = try int_of_string char#text with _ -> 0 in
        let where =
          if String.trim line#text = "" then begin
            let offset = max 0 (min char buf#end_iter#offset) in
            buf#get_iter (`OFFSET offset)
          end else begin
            let line = (int_of_string line#text) - 1 in
            let where = buf#get_iter (`LINE line) in
            let char = max 0 (min (where#chars_in_line - 1) char) in
            buf#get_iter (`LINECHAR (line, char))
          end;
        in
        view#buffer#place_cursor ~where;
        view#scroll_lazy where;
        w#destroy();
      with e -> Dialog.display_exn ~parent:view e
    in
    button_ok#connect#clicked ~callback:(fun () -> callback(); w#destroy()) |> ignore;
    button_cancel#connect#clicked ~callback:w#destroy |> ignore;
    w#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _Return then begin
        callback();
        true
      end else if key = _Escape then begin
        w#destroy();
        true
      end else false;
    end |> ignore;
    line#misc#grab_focus();
    Gaux.may ~f:(fun x -> w#set_transient_for x#as_window) (GWindow.toplevel view);
    w#present()
  end with e -> Dialog.display_exn ~parent:view e;
