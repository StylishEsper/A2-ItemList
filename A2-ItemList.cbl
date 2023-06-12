       identification division.
       program-id. A2-ItemList as "A2_ItemList.A2-ItemList".
       author. Ahmed Butt.
       date-written. 2021-01-28.

       environment division.
       configuration section.

       input-output section.
       file-control.
      
           select input-file
               assign to "../../../A2-ItemList/A2.dat"
               organization is line sequential.
      
           select output-file
               assign to "../../../A2-ItemList/A2.out"
               organization is line sequential.
      
       data division.
       file section.

       fd input-file
           data record is input-line
           record contains 27 characters.

       01 input-line.
         05 il-item-number pic x(4).
         05 il-class pic x.
         05 il-item-name pic x(13).
         05 il-qty pic 999.
         05 il-unit-price pic 9999v99.

       fd output-file
           data record is output-line
           record contains 108 characters.

       01 output-line pic x(108).

       working-storage section.

       01 ws-flag pic x value "n".

       01 ws-my-info.
         05 filler pic x(94) value spaces.
         05 ws-info pic x(14) value "Ahmed Butt, A2".

       01 ws-sections pic x(108) value "----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+---".
       01 ws-heading1 pic x(108) value " ITEM     ITEM      QTY     UNIT       EXTENDED       DISCOUNT      NET PRICE  CLASS  TRANS   TRANSPORTATION".
       01 ws-heading2 pic x(108) value "  #    DESCRIPTION          PRICE       PRICE          AMOUNT                          %          CHARGE    ".

       01 ws-data-line.
         05 filler pic x value spaces.
         05 ws-item-number pic x(4).
         05 filler pic x(2) value spaces.
         05 ws-item-name pic x(13).
         05 filler pic x value spaces.
         05 ws-qty pic zzz.
         05 filler pic x(4) value spaces.
         05 ws-unit-price pic zzz9.99.
         05 filler pic x(3) value spaces.
         05 ws-extended-price pic z,zzz,zz9.99.
         05 filler pic x(3) value spaces.
         05 ws-discount-amount pic zzz,zz9.99.
         05 filler pic x(3) value spaces.
         05 ws-net-price pic z,zzz,zz9.99.
         05 filler pic x(4) value spaces.
         05 ws-class pic x.
         05 filler pic x(4) value spaces.
         05 ws-trans pic z9.9.
         05 ws-percent pic x value "".
         05 filler pic x(4) value spaces.
         05 ws-transportation-charge pic z,zzz,zz9.99.

       01 ws-totals.
         05 filler pic x(37) value spaces.
         05 filler pic x value "$".
         05 ws-total-ext pic zz,zzz,zz9.99.
         05 filler pic x(14) value spaces.
         05 filler pic x value "$".
         05 ws-total-net pic z,zzz,zz9.99.
         05 filler pic x(17) value spaces.
         05 filler pic x value "$".
         05 ws-total-transp pic z,zzz,zz9.99.

       01 ws-without-discount.
         05 filler pic x(25) value "ITEMS WITHOUT DISCOUNT = ".
         05 ws-total-without pic z9.9.
         05 filler pic x value "%".

       01 ws-total-ext-calc pic 99999999v99.
       01 ws-total-net-calc pic 99999999v99.
       01 ws-total-transp-calc pic 99999999v99.
       01 ws-total-without-calc pic 99.
       01 ws-total-without-100 pic 99v9999.
       01 ws-total-items pic 99.


       01 ws-qty-calc pic 999.
       01 ws-unit-calc pic 9999v99.
       01 ws-extended-calc pic 9999999v99.
       01 ws-net-calc pic 9999999v99.
       01 ws-discount-calc pic 999999v99.
       01 ws-transp-calc pic 9999999v99.

       procedure division.
       000-main.

           open input input-file.
           open output output-file.

           display ws-my-info.
           display "".
           display ws-sections.
           display ws-heading1.
           display ws-heading2.
           display "".
           display "".

           write output-line from ws-my-info.
           write output-line from "".
           write output-line from ws-sections.
           write output-line from ws-heading1.
           write output-line from ws-heading2.
           write output-line from "".
           write output-line from "".

           read input-file
               at end
                   move "y" to ws-flag.

           perform loopy-loopy
             until ws-flag equals "y".

           display "".
           write output-line from "".

           move ws-total-ext-calc to ws-total-ext.
           move ws-total-net-calc to ws-total-net.
           move ws-total-transp-calc to ws-total-transp.

           display ws-totals.
           display "".
           display "".
           write output-line from ws-totals.
           write output-line from "".
           write output-line from "".

           subtract ws-total-without-calc from ws-total-items giving ws-total-without-calc.
           divide ws-total-without-calc by ws-total-items giving ws-total-without-100 rounded.
           multiply ws-total-without-100 by 100 giving ws-total-without rounded.

           display ws-without-discount.
           write output-line from ws-without-discount.

           close output-file.

           accept return-code.

           goback.

       loopy-loopy.

           move spaces to ws-data-line.
           move 0 to ws-discount-amount.
           move 0 to ws-qty-calc.
           move 0 to ws-unit-calc.
           move 0 to ws-extended-calc.
           move 0 to ws-discount-calc.

           add 1 to ws-total-items.

           move il-item-number to ws-item-number.
           move il-item-name to ws-item-name.
           move il-qty to ws-qty.
           move il-qty to ws-qty-calc.
           move il-unit-price to ws-unit-price.
           move il-unit-price to ws-unit-calc.
           move il-class to ws-class.
           move "%" to ws-percent.

           multiply ws-qty-calc by ws-unit-calc giving ws-extended-calc.
           move ws-extended-calc to ws-extended-price.

           if ws-class = "A" and ws-extended-calc > 100
             then
               multiply ws-extended-calc by 0.05 giving ws-discount-amount
               multiply ws-extended-calc by 0.05 giving ws-discount-calc
               add 1 to ws-total-without-calc
           end-if.

           if ws-class = "F" and ws-extended-calc > 50
             then
               multiply ws-extended-calc by 0.05 giving ws-discount-amount
               multiply ws-extended-calc by 0.05 giving ws-discount-calc
               add 1 to ws-total-without-calc
           end-if.

           if ws-class = "B" and ws-qty-calc > 5
             then
               multiply ws-extended-calc by 0.05 giving ws-discount-amount
               multiply ws-extended-calc by 0.05 giving ws-discount-calc
               add 1 to ws-total-without-calc
           end-if.

           subtract ws-discount-calc from ws-extended-calc giving ws-net-calc.
           move ws-net-calc to ws-net-price

           if ws-class = "A"
             then
               move 12.5 to ws-trans
               multiply ws-extended-calc by 0.125 giving ws-transp-calc
               move ws-transp-calc to ws-transportation-charge
           end-if

           if ws-class = "D"
             then
               move 8.5 to ws-trans
               multiply ws-extended-calc by 0.085 giving ws-transp-calc
               move ws-transp-calc to ws-transportation-charge
           end-if

           if ws-class = "F"
             then
               move 4.5 to ws-trans
               multiply ws-extended-calc by 0.045 giving ws-transp-calc
               move ws-transp-calc to ws-transportation-charge
           else
               if ws-qty <= 100 and ws-class <> "F" and ws-class <> "D" and ws-class <> "A"
                 then
                   move 6.5 to ws-trans
                   multiply ws-extended-calc by 0.065 giving ws-transp-calc
                   move ws-transp-calc to ws-transportation-charge
               else
                   if ws-class <> "F" and ws-class <> "D" and ws-class <> "A"
                     then
                       move 0 to ws-trans
                       move 45 to ws-transp-calc
                       move ws-transp-calc to ws-transportation-charge
                   end-if
               end-if
           end-if

           add ws-extended-calc to ws-total-ext-calc.
           add ws-net-calc to ws-total-net-calc.
           add ws-transp-calc to ws-total-transp-calc

           display ws-data-line.
           display "".

           write output-line from ws-data-line.
           write output-line from "".

           read input-file
               at end
                   move "y" to ws-flag.
          
       end program A2-ItemList.
