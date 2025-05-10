let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 10);
  set(code,2,swap);
  set(code,3,quote_int 4);
  set(code,4,cons);
  set(code,5,sub);
;;
