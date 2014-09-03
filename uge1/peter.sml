fun powernew (a, 0)= 1
  | powernew (a, 2)  = a * a
  | powernew  (a, n)=  if (n mod 2)=1 then a * powernew (a, n-1)
                       else powernew( powernew(a, (n div 2)),2);
  
