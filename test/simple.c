#include <stdio.h> 
#include <assert.h>
#include <math.h>
#include <stdlib.h>

    extern void __VERIFIER_error(int);

	// inputs
	int inputs[] = {1,2};

	void calculate_output(int);
	void calculate_outputm1(int);

	int a1 = 10;

 void calculate_outputm1(int input) {
    if((( cf==1  && a1 == 10) && (input == 1))) {
    	cf = 0;
    	a1 = 20;
    	 printf("%d\n", 21); fflush(stdout); 
    }
    if((( cf==1  && a1 == 10) && (input == 2))) {
    	cf = 0;
    	 printf("%d\n", 22); fflush(stdout); 
    }  
    if(a1 == 20 && (((input == 2) &&  cf==1 ))) {
    	cf = 0;
    	a1 = 10; 
    	 printf("%d\n", 19); fflush(stdout); 
    }
    if(a1 == 20 && (((input == 1) &&  cf==1 ))) {
    	cf = 0;
    	 printf("%d\n", 20); fflush(stdout); 
    }  
}

 void calculate_output(int input) {
    cf = 1;

    if( cf==1 ) {
    	calculate_outputm1(input);   	
    } 

    if( cf==1 ) 
    	fprintf(stderr, "Invalid input: %d\n", input); 
}

int main()
{
    // main i/o-loop
    while(1)
    {
        // read input
        int input;
        scanf("%d", &input);        
        // operate eca engine
        if((input != 1) && (input != 2))
          return -2;
        calculate_output(input);
    }
}
