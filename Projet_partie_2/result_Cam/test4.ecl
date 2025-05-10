let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_null());
  set(code,2,cons);
  set(code,3,push);
  set(code,4,cur 5);
  set(code,5,cdr);
  set(code,6,swap);
  set(code,7,rplac);
  set(code,8,push);
  set(code,9,cdr);
  set(code,10,swap);
  set(code,11,quote_int 3);
  set(code,12,cons);
  set(code,13,app);
;;
