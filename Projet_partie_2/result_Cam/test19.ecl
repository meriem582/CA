let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,push);
  set(code,2,cur 3);
  set(code,3,push);
  set(code,4,cdr);
  set(code,5,swap);
  set(code,6,quote_int 1);
  set(code,7,cons);
  set(code,8,add);
  set(code,9,swap);
  set(code,10,quote_int 5);
  set(code,11,cons);
  set(code,12,app);
  set(code,13,cons);
  set(code,14,push);
  set(code,15,cdr);
  set(code,16,swap);
  set(code,17,quote_int 2);
  set(code,18,cons);
  set(code,19,mult);
;;
