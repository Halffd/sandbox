program SimpleBooleanInvert;

uses
  SysUtils;

var
  inputBool: Boolean;
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

  // Simply invert the Boolean value
  inputBool := not inputBool;

  if inputBool then
    WriteLn('The result is: TRUE')
  else
    WriteLn('The result is: FALSE');
end.