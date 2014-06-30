Implementation of article

"THE SIZE-CHANGE TERMINATION PRINCIPLE FOR CONSTRUCTOR BASED LANGUAGES"
Pierre Hyvernat  

Available here

http://www.lama.univ-savoie.fr/~hyvernat/research.php
[Hyv14-sct]

----------------------
Compiling the program
----------------------

To compile 
   $ make 

To clean 
   $ make clean

-------------------
Running the program
-------------------

Program reads definitions from a file with extension .sct

$ ./sct functions.sct

For each definition, a message is displayed.

 " function may terminate " if they pass the test
 " Sct can not answered " otherwise

-------------------------------
Optional command line arguments
-------------------------------

--parse-only	  : Only make syntactic analysis

--check-only   	  : Only make checking arity

--print	       	  : Print abstract syntax tree
		    Name of function are all functions meet in the declaration
		    of function, even if is a anonymus function, 
		    i.e., functions not declared previously.

--print-step   	  : Print construction of graph of paths, step by step

--print-self-loop : Print coherent self loop

--print-closure   : Print graph of paths

-B 		  : Specified weight (strictly positive)

-D 		  : Specified depth (positive)

By default B = 1 and D = 2
Changes are applied to all functions in a file

--------------
Grammar of SCT
--------------

The language used is a constructor based language, call sct.
Partial application is not allowed and no type checking is made.

- Constructor -

   All constructors must be declared with keyword "constructor",
 follow by a name and an arity.

 Example : 
   constructor Zero 0
   constructor Succ 1

 They must be declared at the begining of the file.

- Function -

   Function are declared with keyword "let rec" follow by "and" for mutual recursion.

   Example : 
     let rec f x = g x 
     and g x = f x 

- Match / with -

    They must be ended by a "end" 

    Example :      
      match x with 
      | Zero -> Zero
      | Succ n -> n 
      

Many examples are in folder example_sct/

----------
Report bug
----------
filiatr@lri.fr

yohan.chatelain@u-psud.fr

My internship at LRI.