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

RcppExport SEXP combn(SEXP x_, SEXP m_, SEXP n_, SEXP totalComb_, SEXP retmat)
{

	// Convert to appropriate C++ types
	NumericVector x(x_);
	int m = as<int>(m_), n = as<int>(n_), totalComb = as<int>(totalComb_);

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

	// float chunksize;
	// string sched = "static";

	// if (sched == "static") {
	// 	omp_set_schedule(omp_sched_static, chunksize);
	// }
	// else if (sched == "dynamic") {
	// 	omp_set_schedule(omp_sched_dynamic, chunksize);
	// }
	// else if (sched == "guided") {
	// 	omp_set_schedule(omp_sched_guided, chunksize);
	// }
	// else {
	// 	omp_set_schedule(omp_sched_static, chunksize);
	// }

	NumericMatrix ret(m, totalComb);
	int pos = 0;

	#pragma omp parallel
	{
		int me, nth, *comb;

		nth = omp_get_num_threads();
		me = omp_get_thread_num();

		comb = new int[m];

		for (int i = 0; i < m; ++i) {
			comb[i] = i;
		}

		// 	/* Used for checking load balance */
		// if ((n-m+1) <= nth) {
		// 	chunksize = 1;
		// }
		// else {
		// 	chunksize = ceil((float)(n - m + 1)/(float)nth);
		// }
		
		// #pragma omp single
		// {
		// 	cout << "Chunksize: " << chunksize << endl;
		// }

		// while(group <= chunksize){
			
		// 	if (group == 1) {
		// 		current_x = me;
		// 	}
		// 	else if (group % 2 == 0) {
		// 		current_x = current_x+2*(nth-me-1)+1;
		// 	}
		// 	else {
		// 		current_x = current_x + 2 * me+1;
		// 	}

		// 	get_comb_pos(comb, current_x, m);
		// 	while(next_comb(comb, m, temp_n-current_x))  {
		// 		get_comb_pos(comb, current_x, m);
		// 	}
		// 	for(int i = 0; i < m; i++) {
		// 		comb[i] = i;
		// 	}

		// 	#pragma omp critical
		// 	{
		// 		cout << "Thread: " << me << " on " << current_x << endl;
		// 	}

		// 	#pragma omp single
		// 	{
		// 		group++;
		// 	}

		// 	#pragma omp barrier
		// }
		
		int temp_n = n;
		int group = 1;

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
				}
			}

			for(int i = 0; i < m; i++) {
				comb[i] = i;
			}

			group++;
			if (group % 2 == 0) {
				current_x = current_x+2*(nth-me-1);
			}
			else {
				current_x = current_x + 2 * me;
			}
						
		}

	}
		
	return ret;
}
