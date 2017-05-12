/*
 * $Id: BrazilState.cpp 107 2006-07-27 03:31:12Z owen $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */


class PlanningException : public std::exception {

 private:
    std::string msg;
    
 public:
    PlanningException(std::string msg) : msg(msg) {};
    virtual ~PlanningException() throw() {};
	
    virtual const char* what() const throw() { return msg.c_str(); }
	
};
