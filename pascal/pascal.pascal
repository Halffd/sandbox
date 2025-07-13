program PascalTriangle;

function Pascal(row, col: integer): integer;
begin
  if (col = 0) or (col = row) then
    Pascal := 1  { Edge cases are always 1 }
  else if (row > 0) and (col > 0) then
    Pascal := Pascal(row-1, col-1) + Pascal(row-1, col)
  else
    Pascal := 0;
end;

var
  i, j, rows: integer;
begin
  write('Enter number of rows for Pascal''s triangle: ');
  readln(rows);
  
  { Print the triangle }
  for i := 0 to rows - 1 do
  begin
    { Spacing for pretty printing }
    for j := 0 to rows - i - 2 do
      write('  ');
      
    { Print each value in this row }
    for j := 0 to i do
      write(Pascal(i, j):4);
      
    writeln; { New line }
  end;
end.