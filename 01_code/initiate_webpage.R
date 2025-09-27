library(rsconnect)
rsconnect::setAccountInfo(name='sukramreburg',
                          token='5A6A50822FD4AF34E002BCFB9F0DE937',
                          secret='DCPVqPYOasnijIykKJU/oKfeEzSLYwka2YIh/oiJ')
rsconnect::deployApp("./01_code/")
