let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,push);
  set(code,2,quote_int 4);
  set(code,3,swap);
  set(code,4,push);
  set(code,5,quote_int 2);
  set(code,6,swap);
  set(code,7,quote_int 3);
  set(code,8,cons);
  set(code,9,add);
  set(code,10,cons);
  set(code,11,mult);
  set(code,12,cons);
  set(code,13,cdr);
;;
