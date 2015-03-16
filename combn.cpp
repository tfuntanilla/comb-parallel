#include <Rcpp.h>
#include <omp.h>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <vector>

using namespace std;
using namespace Rcpp;

int next_comb(int *comb, int m, int n) {
	int i = m - 1;	
	++comb[i];
		
	while ((i >= 0) && (comb[i] >= n - m + 1 + i)) {		
		--i;		
		++comb[i];	
	}
		
	if(comb[0] == 1) {
		return 0;
	}
		
	for (i = i + 1; i < m; ++i) {
		comb[i] = comb[i - 1] + 1;		
	}
	
	return 1;
}

RcppExport SEXP combn
(SEXP x_, SEXP m_, SEXP n_, SEXP totalComb_, SEXP sched_, SEXP chunksize_, SEXP retmat)
{

	// Convert to appropriate C++ types
	NumericVector x(x_); // input vector
	int m = as<int>(m_), n = as<int>(n_), totalComb = as<int>(totalComb_), 
		chunksize  = as<int>(chunksize_);

	string sched = as<string>(sched_);

	// Check that all parameters were passed correctly
	// if (n < 10) {
	// 	cout << "x[]: ";
	// 	for(int i = 0; i < n; ++i) {
	// 		cout << x[i] << " ";
	// 	}
	// 	cout << endl;
	// }
	// cout << "m: " << m << endl;
	// cout << "n: " << n << endl;

	// output matrix
	NumericMatrix ret(m, totalComb);
	int pos = 0; // used for making storing output in the output matrix

	if (sched == "dynamic") {
		omp_set_schedule(omp_sched_dynamic, chunksize);
	}
	else if (sched == "guided") {
		omp_set_schedule(omp_sched_guided, chunksize);
	}
	// else don't set any OpenMP scheduling and run default

	#pragma omp parallel
	{
		int me, nth;

		nth = omp_get_num_threads();
		me = omp_get_thread_num();

		int *comb = new int[m];

		for (int i = 0; i < m; ++i) {
			comb[i] = i;
		}

	// 		/* Used for checking load balance */
	// 	int group = 1;
	// 	if ((n-m+1) <= nth) {
	// 		chunksize = 1;
	// 	}
	// 	else {
	// 		chunksize = ceil((float)(n - m + 1)/(float)nth);
	// 	}
		
	// 	#pragma omp single
	// 	{
	// 		cout << "Chunksize: " << chunksize << endl;
	// 	}
	// 	int current_x;
	// 	while(group <= chunksize){
			
	// 		if (group == 1) {
	// 			current_x = me;
	// 		}
	// 		else if (group % 2 == 0) {
	// 			current_x = current_x+2*(nth-me-1)+1;
	// 		}
	// 		else {
	// 			current_x = current_x + 2 * me+1;
	// 		}

	// 		#pragma omp critical
	// 		{
	// 			cout << "Thread: " << me << " on " << current_x << endl;
	// 		}

	// 		#pragma omp single
	// 		{
	// 			group++;
	// 		}

	// 		#pragma omp barrier
	// 	}
		int temp_n = n;
		if ((sched == "dynamic") || (sched == "guided")) {
			#pragma omp for schedule(runtime)
			for(int current_x = me; current_x < n-m+1; current_x+=1) {
				int temp;
				#pragma omp critical
				{	
					for (int i = 0; i < m; ++i) {
						temp = comb[i] + current_x;
						ret(i, pos) = x[temp];
					}
					pos++;
				}
				while(next_comb(comb, m, temp_n-current_x))  {
					int temp;
					#pragma omp critical
					{
						for (int i = 0; i < m; ++i) {
							temp = comb[i] + current_x;
							ret(i, pos) = x[temp];
						}
						pos++;
					}
					for(int i = 0; i < m; i++) {
						comb[i] = i;
					}
				}
			}
		}
		else { // static scheduling
			int chunk = 1;

			for(int current_x = me; current_x < n-m+1; current_x+=1) {
				
				int temp;
				for (int i = 0; i < m; ++i) {
					temp = comb[i] + current_x;
					ret(i, pos) = x[temp];
				}
				pos++;
				
				while(next_comb(comb, m, temp_n-current_x))  {
					int temp;
					for (int i = 0; i < m; ++i) {
						temp = comb[i] + current_x;
						ret(i, pos) = x[temp];
					}
					pos++;
				}

				for(int i = 0; i < m; i++) {
					comb[i] = i;
				}

				chunk++;
				if (chunk % 2 == 0) { // even distribution
					current_x = current_x + 2 * (nth - me - 1);
				}
				else { // odd distribution
					current_x = current_x + 2 * me;
				}
				
			}
		}
	}

	return ret;
}
