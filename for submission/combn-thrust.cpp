/*********************************************************************************
Thrust (C++) implementation of R's combn() function from the CRAN combinat package

Called from combn-thrust.R using .Calll() through Rcpp interface
**********************************************************************************/
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/random.h>
#include <stdlib.h>
#include <thrust/transform.h>
#include <stdio.h>
#include <boost/math/special_functions/binomial.hpp>

#include <Rcpp.h>

using namespace std; 
using namespace Rcpp;


struct comb {
	
	const thrust::device_vector<int>::iterator x;
	const thrust::device_vector<int>::iterator pos;
	const thrust::device_vector<int>::iterator retmat;
	int n;
	int m;
	int *x_arr, *position, *ret;


	comb(thrust::device_vector<int>::iterator _x_arr, thrust::device_vector<int>::iterator _pos, int _n, int _m, thrust::device_vector<int>::iterator _retmat):
		x(_x_arr),
		pos(_pos),
		n(_n),
		m(_m),
		retmat(_retmat)
	{
		x_arr = thrust::raw_pointer_cast(&x[0]);		
		position = thrust::raw_pointer_cast(&pos[0]);
		ret = thrust::raw_pointer_cast(&retmat[0]);
	}
	
	__device__
	void operator()(int i)
	{
        if(i <= n - m)
        {
            find_comb(i, x_arr, m, n, position, ret);
        }
	}
	__device__
	void store(int *pos, int *output, int idx, int m, int *x, int *comb, int &outidx){
		for(int i = 0; i < m; i++){
			output[outidx++] = x[comb[i]+idx];
		}

	}

	__device__
	void find_comb(int idx, int *x, int m, int n, int *pos, int *output){
		int *comb = new int[m];
		for(int i = 0; i < m; i++){
			comb[i] = i;
		}
		int new_n= n - idx;
        
        int outidx = pos[idx];
        
		store(pos, output, idx, m, x, comb, outidx);

		while(true){
			int i = m - 1;
			++comb[i];
	
			while((i >= 0) && (comb[i] >= new_n - m + 1 + i)){
				--i;
				++comb[i];
			}
			if(comb[0] == 1){
				break;
			}
			for(i = i + 1; i < m; ++i){
				comb[i] = comb[i-1] + 1;
			}
            store(pos, output, idx, m, x, comb, outidx);
		}
	}

};

RcppExport SEXP combnthrust(SEXP x_, SEXP m_, SEXP n_, SEXP nCm_, SEXP out){
	NumericVector x(x_);
	int m = as<int>(m_), n = as<int>(n_), nCm = as<int>(nCm_);
	NumericMatrix retmat(m, nCm);

    // keeps track of the position in output
    int *pos = new int[n-m+1];
    
    // Calculate combination possibilities for each element in the list that
    // start with the element in the 0th index
    int k = 0;
    for(int i = 0; i < (n-m+1); i++){
        pos[i] = boost::math::binomial_coefficient<double>(n - i - 1, m-1);
        k++;
    }
    // Calcluate the position vector respective to the possiblities
    int temp = pos[0]*m;
    int temp2;
    pos[0]=0;
    for (int j=1; j<(n-m+1); j++){
        temp2 = pos[j];
        pos[j]=temp;
        temp=pos[j]+(m*temp2);
    }
	
	thrust::device_vector<int> d_x(x.begin(), x.end());
    thrust::device_vector<int> d_pos(pos, pos + (n-m+1));
	thrust::device_vector<int> d_mat(retmat.begin(), retmat.end());
    
	thrust::counting_iterator<int> begin(0);
	thrust::counting_iterator<int> end = begin + n;

	thrust::for_each(begin, end, comb(d_x.begin(), d_pos.begin(), n, m, d_mat.begin()));

	thrust::copy(d_mat.begin(), d_mat.end(), retmat.begin());
	
	return retmat;
    
}
