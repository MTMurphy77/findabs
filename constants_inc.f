      double precision amu,c,e,epsilon_0,G,hbar,k_b
      double precision m_e,m_p,m_n,pi,sqrtpi,mu_0
      double precision secperyr,lightyrperparsec
c All constants in SI units.
c All taken from SI Chemical Data (3rd Ed.)
      parameter (amu=1.6605402d-27,c=299792458.d0,e=1.60217733d-19)
      parameter (epsilon_0=8.854187816d-12,G=6.67d-11)
c This is what people usually quote for G - the error is
c probably at the fractional level of 10-3
      parameter (hbar=1.05457266d-34,k_B=1.380658d-23)
      parameter (m_e=9.1093897d-31,m_p=1.6726231d-27,m_n=1.6749286d-27)
      parameter (pi=3.141592653590d0,sqrtpi=1.772453850906d0)
      parameter (mu_0=4.d0*pi*1.d-7)
      parameter (secperyr=3.1557d7)
c This is just 60*60*24*365.2422 ... This may or may not be correct
      parameter (lightyrperparsec=3.26d0)
