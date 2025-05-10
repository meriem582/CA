let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,cur 2);
  set(code,2,push);
  set(code,3,quote_int 3);
  set(code,4,swap);
  set(code,5,cdr);
  set(code,6,cons);
  set(code,7,mult);
  set(code,8,cons);
  set(code,9,push);
  set(code,10,cdr);
  set(code,11,swap);
  set(code,12,quote_int 7);
  set(code,13,cons);
  set(code,14,app);
;;
