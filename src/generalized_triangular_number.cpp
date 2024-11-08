
#include<cmath>

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
ComplexVector generalized_triangular_numbers_point(const ComplexVector& z, const int& n){
    ComplexVector coeff(1);
    double eps = 1.0e-30; Rcomplex z_value = z[0];
    if(n == 1){
        coeff[0].r = 1.0; coeff[0].i = 0.0; 
        return coeff;
    } else if(std::abs(z_value.r) < eps && std::abs(z_value.i) < eps){
        coeff[0].r = 0.0; coeff[0].i = 0.0; 
        return coeff;
    }
    double coeff_real = 1.0; double coeff_imag = 0.0; double coeff_temp;
    double coeff_ratio_real; double coeff_ratio_imag;
    for(int i = 1; i < n; ++i){
        coeff_ratio_real = ((z_value.r+i-1)/i); coeff_ratio_imag = (z_value.i/i);
        coeff_temp = coeff_real;
        coeff_real = coeff_real*coeff_ratio_real-coeff_imag*coeff_ratio_imag;
        coeff_imag = coeff_temp*coeff_ratio_imag+coeff_imag*coeff_ratio_real;
    }
    coeff[0].r = coeff_real; coeff[0].i = coeff_imag;
    return coeff;
}

// [[Rcpp::export]]
ComplexVector generalized_triangular_numbers_sequence(const ComplexVector& z, const int& n){
    ComplexVector coeff(n);
    coeff[0].r = 1.0; coeff[0].i = 0.0;
    double eps = 1.0e-30; Rcomplex z_value = z[0];
    if((std::abs(z_value.r) < eps && std::abs(z_value.i) < eps) || n == 1){
        return coeff;
    }
    double coeff_real = 1.0; double coeff_imag = 0.0; double coeff_temp;
    double coeff_ratio_real; double coeff_ratio_imag;
    for(int i = 1; i < n; ++i){
        coeff_ratio_real = ((z_value.r+i-1)/i); coeff_ratio_imag = (z_value.i/i);
        coeff_temp = coeff_real;
        coeff_real = coeff_real*coeff_ratio_real-coeff_imag*coeff_ratio_imag;
        coeff_imag = coeff_temp*coeff_ratio_imag+coeff_imag*coeff_ratio_real;
        coeff[i].r = coeff_real; coeff[i].i = coeff_imag;
    }
    return coeff;
}

