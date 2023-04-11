# -*- coding: utf-8 -*-
"""
Created on Thu Mar 30 14:31:50 2023

@author: loren
"""
x = ["ReportingTime",
     "RecentWorkLoad",
     "FlightDutyPeriod",
     "DayNightFlight",
     "PreFlightSleep",
     "RestOnFlight",
     "OperationalLoad",
     "PreFlightCrewFitness",
     "InFlightCrewAlertness"]

options = [["Morning", "Afternoon","Evening"],
           ["Light", "Heavy"],
           ["Short","Long"],
           ["Day","Night"],
           ["Poor","Good"],
           ["Poor","Good"],
           ["Average","High"],
           ["OK","NotOK"],
           ["Low","Average"]]

ouders = {"ReportingTime":[],
           "RecentWorkLoad":[],
           "FlightDutyPeriod":[],
           "DayNightFlight":[],
           "PreFlightSleep":[0],
           "RestOnFlight":[2],
           "OperationalLoad":[2,5],
           "PreFlightCrewFitness":[1,4],
           "InFlightCrewAlertness":[3,6,7]}

probabilities = {"ReportingTime":{"Morning":0.4,"Afternoon":0.2,"Evening":0.4},
                 "RecentWorkLoad":{"Light":0.88,"Heavy":0.12},
                 "FlightDutyPeriod":{"Short":0.22,"Long":0.78},
                 "DayNightFlight":{"Day":0.6,"Night":0.4},
                 "PreFlightSleep":{"Poor":{"Morning":0.7,"Afternoon":0.59,"Evening":0.5},"Good":{"Morning":0.3,"Afternoon":0.41,"Evening":0.5}},
                 "RestOnFlight":{"Poor":{"Short":0.1,"Long":0.4},"Good":{"Short":0.9,"Long":0.6}},
                 "OperationalLoad":{"Average":{"Short":{"Poor":0.2,"Good":1.0},"Long":{"Poor":0.1,"Good":0.3}},"High":{"Short":{"Poor":0.8,"Good":0.0},"Long":{"Poor":0.9,"Good":0.7}   }   },
                 "PreFlightCrewFitness":{"OK":{"Light":{"Poor":0.55,"Good":0.86},"Heavy":{"Poor":0.24,"Good":0.77}},"NotOK":{"Light":{"Poor":0.45,"Good":0.14},"Heavy":{"Poor":0.76,"Good":0.23}}},
                 "InFlightCrewAlertness":{"Low":{"Day":{"Average":{"OK":0.2,"NotOK":0.3},"High":{"OK":0.15,"NotOK":0.33}},"Night":{"Average":{"OK":0.0,"NotOK":0.6},"High":{"OK":0.3,"NotOK":0.7}}},"Average":{"Day":{"Average":{"OK":0.8,"NotOK":0.7},"High":{"OK":0.85,"NotOK":0.67}},"Night":{"Average":{"OK":1.0,"NotOK":0.4},"High":{"OK":0.7,"NotOK":0.3}}}}}



def jointprob():
    xs = [None] * len(x)
    xs[0] = str(input("x0 (Morning, Afternoon, Evening) = "))
    xs[1] = str(input("x1 (Light, Heavy) = "))
    xs[2] = str(input("x2 (Short, Long) = "))
    xs[3] = str(input("x3 (Day, Night) = "))
    xs[4] = str(input("x4 (Poor, Good) = "))
    xs[5] = str(input("x5 (Poor, Good) = "))
    xs[6] = str(input("x6 (Average, High) = "))
    xs[7] = str(input("x7 (OK, NotOK) = "))
    xs[8] = str(input("x8 ( Low, Average) = "))
    
    product = 1
    for index in range(0, 3+1):
        product = product * probabilities[x[index]][xs[index]]
    
    for index in range(4, 5+1):
        product = product * probabilities[x[index]][xs[index]][xs[ouders[x[index]][0]]]

    for index in range(6, 7+1):
        product = product * probabilities[x[index]][xs[index]][xs[ouders[x[index]][0]]][xs[ouders[x[index]][1]]]
        
    for index in range(8, 8+1):
        product = product * probabilities[x[index]][xs[index]][xs[ouders[x[index]][0]]][xs[ouders[x[index]][1]]][xs[ouders[x[index]][2]]]
    
    return product


def margprob(var = 8,opt = ""):
    var = int(var)
    if var < 0 or var > 8:
        return "Not a valid index"
    
    if opt == "":
        xs = str( input( str(x[var])+" = " ))
    else:
        xs = opt
        
    if var < 4:
        prob = probabilities[x[var]][xs]
        
    elif var < 6:
        prob = 0
        parent = ouders[x[var]][0]
        for optionouder in options[parent]:
            prob += probabilities[x[var]][xs][optionouder]*margprob(parent,optionouder)
            
    elif var == 6:
        prob = 0
        parents = ouders[x[var]]   
        for option1 in options[parents[0]]:
            for option2 in options[parents[1]]:
                prob += probabilities[x[var]][xs][option1][option2]*margprob(parents[0],option1)*probabilities[x[parents[1]]][option2][option1]
                
    elif var == 7: 
        prob = 0
        parents = ouders[x[var]]   
        for option1 in options[parents[0]]:
            for option2 in options[parents[1]]:
                prob += probabilities[x[var]][xs][option1][option2]*margprob(parents[0],option1)*margprob(parents[1],option2)
                
    elif var == 8: 
        prob = 0
        parents = ouders[x[var]]   
        for option1 in options[parents[0]]:
            for option2 in options[parents[1]]:
                for option3 in options[parents[2]]:
                    prob += probabilities[x[var]][xs][option1][option2][option3]*margprob(parents[0],option1)*margprob(parents[1],option2)*margprob(parents[2],option3)
    return prob
    
    
    
    
    
    
    