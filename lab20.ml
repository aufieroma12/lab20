open Graphics ;;
open List ;;
  
(* threshold threshold image -- image where pixels above the threshold
value are black *)

module type IMAGES = 
  sig
  	type image = float list list 
	(* images are lists of lists of floats between 0. (white) and 1. (black) *)
	type size = int * int 

	val map_array : (float -> float) -> image -> image 

	val threshold : image -> float -> image

	val depict : image -> unit

	val dither : image -> image
  end 


module Images : IMAGES = 
  struct
  	type image = float list list 

	type size = int * int 

	let map_array f img =
		map (map f) img ;;

	let threshold img threshold =
		map_array (fun value -> if value <= threshold then 0. else 1.) img ;;
	       
	(* show the image *)
	let depict img =
	  open_graph ""; 
	  clear_graph ();
	  let x, y = length (hd img), length img 
	  in resize_window x y;
	  let depict_pix value row column = 
	  	let lvl = int_of_float (255. *. (1. -. value)) 
	    in set_color (rgb lvl lvl lvl);
		plot column (y - row) in
	  iteri (fun r row -> iteri (fun c pix -> depict_pix pix r c) row) img;
	  Unix.sleep 2; 
	  close_graph () ;;

	(* dither max image -- dithered image *)
	let dither img =
	  map_array (fun value -> if value > Random.float 1. then 1. else 0.) img
  end




let display_images () =
	let mona = Monalisa.image in
	Images.depict mona ;
	Images.depict (Images.threshold mona 0.75);
	Images.depict (Images.dither mona) ;;

display_images () ;;
           
