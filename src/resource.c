#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Just add a number by two to test */
/* TODO: Arguments: RESOURCE_1, RESOURCE_2, VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE_1, SEXP RESOURCE_2, SEXP LANDSCAPE){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;
    int land_x, land_y;
    int individual, trait;
    int individual_number_1, trait_number_1;
    int individuals_added_1, individuals_total_1;
    int protected_n;
    int *dim_RESOURCE_1;
    int *dim_LANDSCAPE;
    double *R_ptr_1;
    double *R_ptr_1_new;
    double *land_ptr;
    int **individuals_old_1;
    int **individuals_new_1;
    int **land;
    int vec_pos;

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT( RESOURCE_1 = AS_NUMERIC(RESOURCE_1) );
    protected_n++;
    R_ptr_1 = REAL(RESOURCE_1);
    
    PROTECT( LANDSCAPE = AS_NUMERIC(LANDSCAPE) );
    protected_n++;
    land_ptr = REAL(LANDSCAPE);
    
    dim_RESOURCE_1 = INTEGER( GET_DIM(RESOURCE_1) );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    
    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    /* Code below remakes the RESOURCE_1 matrix for easier use */
    individual_number_1 = dim_RESOURCE_1[0];
    trait_number_1      = dim_RESOURCE_1[1];
    individuals_old_1   = malloc(individual_number_1 * sizeof(int *));
    for(individual = 0; individual < individual_number_1; individual++){
        individuals_old_1[individual] = malloc(trait_number_1 * sizeof(int));   
    } /* RESOURCE_1 is now stored as individuals_old_1 (discrete resources) */

    /* Code below reads in the LANDSCAPE for easy of use */
    land_y = dim_LANDSCAPE[0];
    land_x = dim_LANDSCAPE[1];
    land   = malloc(land_y * sizeof(int *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(int));   
    } /* LANDSCAPE is now stored as land */
    
    
    
        
    individuals_added_1 = 0; /* TODO: Add a birth function here */
    individuals_total_1 = individual_number_1 + individuals_added_1;

    /* Below makes a new array for new Resource_1, then adds it */
    individuals_new_1 = malloc(individuals_total_1 * sizeof(int *));
    for(individual = 0; individual < individuals_total_1; individual++){
        individuals_new_1[individual] = malloc(trait_number_1 * sizeof(int));   
    }    
    
    for(trait=0; trait<trait_number_1; trait++){
        for(individual=0; individual<individuals_total_1; individual++){
            individuals_new_1[individual][trait] = individual + trait;
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP RESOURCE_1_NEW;
    PROTECT( RESOURCE_1_NEW = allocMatrix(REALSXP, individuals_total_1, 
                                          trait_number_1) );
    protected_n++;
    
    R_ptr_1_new = REAL(RESOURCE_1_NEW);

    vec_pos = 0;
    for(trait=0; trait<trait_number_1; trait++){
        for(individual=0; individual<individuals_total_1; individual++){
            R_ptr_1_new[vec_pos] = individuals_new_1[individual][trait] + 10;
            vec_pos++;
        }
    }            
        
        
    UNPROTECT(protected_n);
    
    return(RESOURCE_1_NEW); 
}
