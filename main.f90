program LUE_MODEL
      implicit none
      
      real,parameter:: LUE_star=0.08 ! g C(muE m-2 s-1)-1
      real:: PAR=1500, FAPAR=0.7, Ta=25., Tmax=35., Tmin=0.0
      real:: VPD=2000.0,VPDmax=6000.0,VPDmin=200.0 ! unit in Pa
      real:: GPP
          
      GPP = PAR * FAPAR * LUE_star * St(Ta,Tmax,Tmin) * Sw(VPD,VPDmax,VPDmin)

      print*,'GPP= ',GPP,'St= ',St(Ta,Tmax,Tmin),'Sw= ',Sw(VPD,VPDmax,VPDmin)

contains
      function St(Ta,Tmax,Tmin)
        real:: Ta, Tmax, Tmin, St
        if (Ta>=Tmax) then
            St=1.
        else if (Ta<=Tmin) then 
            St=0.
        else
            St= (Ta-Tmin)/(Tmax-Tmin)
        end if
      end function st

     function Sw(VPD,VPDmax,VPDmin)
        real:: VPD, VPDmax, VPDmin, Sw
        if (VPD>=VPDmax) then
            Sw=0.
        else if (VPD<=VPDmin) then 
            Sw=1.
        else
            Sw= (VPD-VPDmin)/(VPDmax-VPDmin)
        end if
      end function sw

end program LUE_MODEL
