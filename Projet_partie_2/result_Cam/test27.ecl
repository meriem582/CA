let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,push);
  set(code,2,quote_int 1);
  set(code,3,swap);
  set(code,4,quote_int 2);
  set(code,5,cons);
  set(code,6,cons);
  set(code,7,push);
  set(code,8,cdr);
  set(code,9,cons);
  set(code,10,push);
  set(code,11,cdr);
  set(code,12,car);
  set(code,13,swap);
  set(code,14,cdr);
  set(code,15,cdr);
  set(code,16,cons);
  set(code,17,add);
;;
