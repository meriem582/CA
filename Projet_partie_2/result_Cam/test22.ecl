let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,cur 2);
  set(code,2,push);
  set(code,3,cdr);
  set(code,4,cdr);
  set(code,5,swap);
  set(code,6,cdr);
  set(code,7,car);
  set(code,8,cons);
  set(code,9,cons);
  set(code,10,push);
  set(code,11,cdr);
  set(code,12,swap);
  set(code,13,push);
  set(code,14,quote_int 3);
  set(code,15,swap);
  set(code,16,quote_int 4);
  set(code,17,cons);
  set(code,18,cons);
  set(code,19,app);
;;
