let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 5);
  set(code,2,cons);
  set(code,3,push);
  set(code,4,quote_int 10);
  set(code,5,cons);
  set(code,6,push);
  set(code,7,car);
  set(code,8,cdr);
  set(code,9,swap);
  set(code,10,cdr);
  set(code,11,cons);
  set(code,12,add);
;;
