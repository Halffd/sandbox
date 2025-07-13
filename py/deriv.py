def deriv(chain):
    """
    Calculate the derivative of a function chain using the chain rule.
    Functions are separated by ">" to indicate composition.
    """
    dchain = chain.split(">")  # Split the chain for derivatives
    chain = chain.split(">")   # Split the original chain
    
    # Calculate derivatives of each function in the chain
    for i in range(len(dchain)):
        if dchain[i][:3] == "log":
            # Derivative of log_b(x) is 1/(x*ln(b))
            dchain[i] = "(x*ln(" + dchain[i].split(",")[0][4:] + "))^-1"
        elif "^x" in dchain[i]:
            # Derivative of a^x is a^x * ln(a)
            base = dchain[i].split("^")[0]
            dchain[i] = f"ln({base})*{base}^x"
        elif dchain[i] == "sin(x)":
            dchain[i] = "cos(x)"
        elif dchain[i] == "cos(x)":
            dchain[i] = "-sin(x)"
        elif dchain[i] == "cosh(x)":
            dchain[i] = "sinh(x)"
        elif dchain[i] == "sinh(x)":  # Fixed: was duplicating cosh(x) case
            dchain[i] = "cosh(x)"
        #etc
        else:
            # Handle polynomial terms (assuming form of ax^n + bx^m + ...)
            dchain[i] = dchain[i].split("+")
            for j in range(len(dchain[i])):
                exp = dchain[i][j].split("^")[-1]
                coeff = str(float(dchain[i][j].split("^")[0].split("x")[0])*float(exp))
                exp = str(float(exp)-1)
                dchain[i][j] = coeff + "*x^" + exp
            
            # Combine the polynomial terms back together
            for j in range(len(dchain[i])-1):
                dchain[i][j+1] += "+" + dchain[i][j]
            dchain[i] = dchain[i][-1]
    
    # Apply the chain rule in reverse order
    result = []
    for i in range(len(dchain)):
        # Get the derivative at this step
        result.append(dchain[len(dchain)-i-1])
        
        # Add composition with original functions
        for j in range(i+1, len(chain)):
            result[i] = chain[len(chain)-j-1] + ">" + result[i]
    
    # Multiply the derivatives together according to chain rule
    for i in range(len(result)-1):
        result[i+1] += "*(" + result[i] + ")"
    
    return result[-1]
print(deriv("sin(x)"))
