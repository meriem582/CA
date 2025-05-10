let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 6);
  set(code,2,swap);
  set(code,3,push);
  set(code,4,quote_int 7);
  set(code,5,swap);
  set(code,6,quote_int 2);
  set(code,7,cons);
  set(code,8,div);
  set(code,9,cons);
  set(code,10,mult);
;;
