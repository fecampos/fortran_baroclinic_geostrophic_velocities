      elemental subroutine gsw_distance(lon2,lon1,lat2,lat1,dist)

      implicit none

      real, intent(in) :: lon2, lon1, lat2, lat1

      real, intent(out) :: dist

      real, parameter :: pi = 3.1415927, earth_radius = 6371000

      real :: pi180, dlon, dlat, a, angles

      pi180 = pi/180

      dlon = pi180*(lon2-lon1)
      dlat = pi180*(lat2-lat1)

      a = (sin(0.5*dlat))**2 + cos(lat1*pi180)*cos(lat2*pi180)*(sin(0.5*dlon))**2

      angles = 2*atan2(sqrt(a),sqrt(1-a))

      dist = earth_radius*angles

      return

      end subroutine
