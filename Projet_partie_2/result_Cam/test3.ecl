let code = create<1024>() ;;

let load_code() =
  set(code,0,cur 1);
  set(code,1,push);
  set(code,2,cdr);
  set(code,3,swap);
  set(code,4,quote_int 1);
  set(code,5,cons);
  set(code,6,add);
;;
