let item ~label ?image ?stock ?(icon_size=`MENU) ?(show=true) ?packing () =
  let menu_item = GMenu.menu_item ?packing ~show () in
  let hbox = GPack.hbox ~border_width:2 ~spacing:6 ~packing: menu_item#add () in
  hbox#set_halign `START;
  begin match image, stock with
  | Some image, None -> hbox#add image#coerce
  | None, Some stock -> GMisc.image ~stock ~icon_size ~packing: hbox#add () |> ignore
  | Some _, Some _   -> failwith "Only one of ~image/~stock is allowed"
  | None, None -> ()
  end;
  GMisc.label ~text: label ~packing: hbox#add () |> ignore;
  menu_item
;;
