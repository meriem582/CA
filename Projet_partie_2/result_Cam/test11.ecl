let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,push);
  set(code,2,quote_int 3);
  set(code,3,swap);
  set(code,4,quote_int 4);
  set(code,5,cons);
  set(code,6,cons);
  set(code,7,cdr);
  set(code,8,car);
;;
