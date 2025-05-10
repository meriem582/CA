let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,cur 2);
  set(code,2,push);
  set(code,3,cdr);
  set(code,4,swap);
  set(code,5,cdr);
  set(code,6,cons);
  set(code,7,mult);
  set(code,8,swap);
  set(code,9,quote_int 5);
  set(code,10,cons);
  set(code,11,app);
;;
