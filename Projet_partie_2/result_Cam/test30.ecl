let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 3);
  set(code,2,cons);
  set(code,3,push);
  set(code,4,cdr);
  set(code,5,swap);
  set(code,6,push);
  set(code,7,cdr);
  set(code,8,swap);
  set(code,9,quote_int 1);
  set(code,10,cons);
  set(code,11,add);
  set(code,12,cons);
;;
