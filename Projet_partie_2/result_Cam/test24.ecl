let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_int 5);
  set(code,2,cons);
  set(code,3,push);
  set(code,4,push);
  set(code,5,cdr);
  set(code,6,swap);
  set(code,7,quote_int 3);
  set(code,8,cons);
  set(code,9,gt);
  set(code,10,branch 11 17);
  set(code,11,push);
  set(code,12,cdr);
  set(code,13,swap);
  set(code,14,quote_int 2);
  set(code,15,cons);
  set(code,16,mult);
  set(code,17,push);
  set(code,18,cdr);
  set(code,19,swap);
  set(code,20,quote_int 2);
  set(code,21,cons);
  set(code,22,div);
;;
