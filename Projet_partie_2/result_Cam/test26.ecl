let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 4);
  set(code,2,cons);
  set(code,3,push);
  set(code,4,push);
  set(code,5,cdr);
  set(code,6,swap);
  set(code,7,quote_int 3);
  set(code,8,cons);
  set(code,9,add);
  set(code,10,cons);
  set(code,11,push);
  set(code,12,car);
  set(code,13,cdr);
  set(code,14,swap);
  set(code,15,cdr);
  set(code,16,cons);
  set(code,17,mult);
;;
