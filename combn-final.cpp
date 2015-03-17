// Test for char in input
// Test for the function in parameter
// Test for the output

// Right now - figure out the position at which the combinations will be stored in
// Trying to return a list instead of matrix


#include <Rcpp.h>
#include <omp.h>
#include <iostream>

using namespace std;
using namespace Rcpp;

// Computes the indices of the next combination to generate 
// The indices then get map to the actual values from the input vector
int next_comb(int *comb, int m, int n)
{
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

RcppExport SEXP combn(SEXP x_, SEXP m_, SEXP n_, SEXP nCm_, SEXP out)
{
	// Convert SEXP variables to appropriate C++ types
	NumericVector x(x_);
	int m = as<int>(m_), n = as<int>(n_), nCm = as<int>(nCm_);

	List retlist; // return list

	#pragma omp parallel
	{
		// this thread id, total number of threads, combination indexes array
		int me, nth, *comb, *combvec;

		nth = omp_get_num_threads();
		me = omp_get_thread_num();

		// array that will hold all of the possible combinations 
		// of size m of the indexes
		comb = new int[m]; 
		combvec = new int[m];

		// initialize comb array
		for (int i = 0; i < m; ++i) {
			comb[i] = i;
		}
		
		int temp_n = n;
		int chunkNum = 1; // the number of chunk that has been distributed
		int combPos = current_x - all the numbers that came before; // the position of a combination in the output
		
		// each thread gets assign a chunk to work on
		// each thread will have about the same number of chunks
		// to work on throughout the lifetime of the program
		for(int current_x = me; current_x < n-m+1; current_x+=1) {	
			int temp;
			for (int i = 0; i < m; ++i) {
				temp = comb[i] + current_x;
				combvec[i] = x[temp];
			}
			NumericVector cv(combvec);
			retlist[combPos] = cv;
			combPos++;
			while(next_comb(comb, m, temp_n-current_x))  {
				int temp;
				for (int i = 0; i < m; ++i) {
					temp = comb[i] + current_x;
					combvec[i] = x[temp];
				}
				NumericVector cv(combvec);
				retlist[combPos] = cv;
				combPos++;
			}

			// reset comb array for the next chunk this thread will work on
			for(int i = 0; i < m; i++) {
				comb[i] = i;
			}

			chunkNum++; // increment chunkNum for the next chunk distribution
			// determine which element this thread will work on
			if (chunkNum % 2 == 0) {
				current_x = current_x + 2 * (nth - me - 1);
			}
			else {
				current_x = current_x + 2 * me;
			}
		}
	}
		
	return retlist;
}
