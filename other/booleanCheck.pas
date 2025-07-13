program ComplicatedBooleanCheck;

uses
  SysUtils;

var
  inputBool: Boolean;

function FlipLogic(val: Boolean; count: Integer): Boolean;
begin
  if count = 0 then
    FlipLogic := val
  else
    FlipLogic := FlipLogic(not val, count - 1);
end;

function ComplexXor(val: Boolean): Boolean;
begin
  ComplexXor := ((val xor not val) xor (val and not val)) or ((val or not val) xor val);
end;

function RecursiveTwist(val: Boolean; layer: Integer): Boolean;
begin
  if layer = 0 then
    RecursiveTwist := val
  else
    RecursiveTwist := (RecursiveTwist(not val, layer - 1) xor (layer mod 2 = 0));
end;

function UltraComplexBoolCheck(val: Boolean): Boolean;
var
  flipped: Boolean;
  xorCheck: Boolean;
  twistResult: Boolean;
begin
  flipped := FlipLogic(val, 17);
  xorCheck := ComplexXor(flipped);
  twistResult := RecursiveTwist(xorCheck, 7);

  UltraComplexBoolCheck := ((twistResult or xorCheck) and not (flipped xor xorCheck))
                         or ((not twistResult and xorCheck) and (val or flipped));
end;

var
  inputStr: string;

begin
  Write('Enter a Boolean value (true/false): ');
  ReadLn(inputStr);

  if SameText(inputStr, 'true') then
    inputBool := True
  else if SameText(inputStr, 'false') then
    inputBool := False
  else
  begin
    WriteLn('Invalid input. Please enter "true" or "false".');
    Exit;
  end;

  if UltraComplexBoolCheck(inputBool) then
    WriteLn('The result is: TRUE')
  else
    WriteLn('The result is: FALSE');
end.