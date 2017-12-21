
      subroutine sma_fortran(period, dataa, smas, n)
      integer pos, n, endd, period
      real dataa(n), smas(n), sma
      do 10 endd = 1, n
         pos = endd
         sma = 0.0
         do 20 while ((endd - pos .lt. period) .and. (pos .ge. 1))
            sma = sma + dataa(pos)
            pos = pos - 1
 20      end do
         if (endd - pos .eq. period) then
            sma = sma / period
         else
            sma = 0
         end if
         smas(endd) = sma
 10   continue
      end
