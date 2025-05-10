let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,cur 2);
  set(code,2,cdr);
  set(code,3,cons);
  set(code,4,push);
  set(code,5,cdr);
  set(code,6,swap);
  set(code,7,push);
  set(code,8,cdr);
  set(code,9,swap);
  set(code,10,quote_int 5);
  set(code,11,cons);
  set(code,12,app);
  set(code,13,cons);
  set(code,14,app);
;;
