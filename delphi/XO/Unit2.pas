unit Unit1;

interface

uses
Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
Dialogs, ExtCtrls, Menus;

type
TForm1 = class(TForm)
Image1: TImage;
MainMenu1: TMainMenu;
N1: TMenuItem;
N2: TMenuItem;
N3: TMenuItem;
procedure FormCreate(Sender: TObject);
procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
procedure N2Click(Sender: TObject);
procedure N3Click(Sender: TObject);
private
    { Private declarations }
public
    { Public declarations }
end;

const
C_CellWH = 50; // Длина и ширина одной клетки
C_CC = 10; // кол-во клеток // = 10
C_CP = 5; // длина ряда символов одного типа, необходимого для победы
C_Val = 3; //оценочный коэффициент

type
TKrestikNolik = (knKrestik,knKrWin,knNolik,knNolWin); // крестик или нолик
TPole = array [0..C_CC-1] of array [0..C_CC-1] of Byte;
TVal = array [0..C_CC-1] of array [0..C_CC-1] of Integer; //значение оценочной функции
                                                            //для каждой клетки поля
var
Form1: TForm1;
Pl: TPole;

procedure PaintKN(S:TKrestikNolik; CellX,CellY:byte);
function isYes(var P:TPole):byte;
function NewNol(var P:TPole):byte;
function Value(P:TPole; a,x,y:byte):Integer;
procedure NewGame;

implementation

{$R *.dfm}

procedure PaintKN(S:TKrestikNolik; CellX,CellY:byte);
begin
Form1.Image1.Canvas.Pen.Width:=3;
Case S of
knKrestik:
begin
Form1.Image1.Canvas.Pen.Color:=clRed;
Form1.Image1.Canvas.MoveTo( (C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.LineTo( 7*(C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.MoveTo( (C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.LineTo( 7*(C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH );
end;

knKrWin:
begin
Form1.Image1.Canvas.Pen.Color:=clGreen;
Form1.Image1.Canvas.MoveTo( (C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.LineTo( 7*(C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.MoveTo( (C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH );
Form1.Image1.Canvas.LineTo( 7*(C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH );
end;

knNolik:
begin
Form1.Image1.Canvas.Pen.Color:=clBlue;
Form1.Image1.Canvas.Brush.Style:=bsClear;
Form1.Image1.Canvas.Ellipse( (C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH,
7*(C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH  );
end;

knNolWin:
begin
Form1.Image1.Canvas.Pen.Color:=clGreen;
Form1.Image1.Canvas.Brush.Style:=bsClear;
Form1.Image1.Canvas.Ellipse( (C_CellWH div 8) + CellX*C_CellWH,
(C_CellWH div 8) + CellY*C_CellWH,
7*(C_CellWH div 8) + CellX*C_CellWH,
7*(C_CellWH div 8) + CellY*C_CellWH  );
end;

End;

end;

function isYes(var P:TPole):byte;
var i,j,x:byte; A:Boolean; k,n:byte;
begin
 // проверяем диагонали
for x:=0 to C_CP do
begin
k:=0; n:=0;
for i:=x to C_CC-1 do
begin
if (p[i-x,i]=1) then
begin
Inc(k);
n:=0;
end
else
if (p[i-x,i]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=i downto i-C_CP+1 do
begin PaintKN(knKrWin,k-x,k) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=i downto i-C_CP+1 do
begin PaintKN(knNolWin,n-x,n) end;
Exit;
end;
end;
k:=0; n:=0;
for i:=x to C_CC-1 do
begin
if (p[i,i-x]=1) then
begin
Inc(k);
n:=0;
end
else
if (p[i,i-x]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=i downto i-C_CP+1 do
begin PaintKN(knKrWin,k,k-x) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=i downto i-C_CP+1 do
begin PaintKN(knNolWin,n,n-x) end;
Exit;
end;
end;
end;
for x:=0 to C_CP do
begin
k:=0; n:=0;
for i:=x to C_CC-1 do
begin
if (p[i-x,C_CC-1-i]=1) then
begin
Inc(k);
n:=0;
end
else
if (p[i-x,C_CC-1-i]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=i downto i-C_CP+1 do
begin PaintKN(knKrWin,k-x,C_CC-1-k) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=i downto i-C_CP+1 do
begin PaintKN(knNolWin,n-x,C_CC-1-n) end;
Exit;
end;
end;
k:=0; n:=0;
for i:=x to C_CC-1 do
begin
if (p[i,C_CC-1-i+x]=1) then
begin
Inc(k);
n:=0;
end
else
if (p[i,C_CC-1-i+x]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=i downto i-C_CP+1 do
begin PaintKN(knKrWin,k,C_CC-1-k+x) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=i downto i-C_CP+1 do
begin PaintKN(knNolWin,n,C_CC-1-n+x) end;
Exit;
end;
end;
end;
for i:=0 to C_CC-1 do
begin
   // вертикаль
k:=0; n:=0;
for j:=0 to C_CC-1 do
begin
if (p[i,j]=1) then
begin
Inc(k);
n:=0;
end
else
if (p[i,j]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=j downto j-C_CP+1 do
begin PaintKN(knKrWin,i,k) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=j downto j-C_CP+1 do
begin PaintKN(knNolWin,i,n) end;
Exit;
end;
end;
   // горизонталь
k:=0; n:=0;
for j:=0 to C_CC-1 do
begin
if p[j,i] = 1 then
begin
Inc(k);
n:=0;
end
else
if (p[j,i]=2) then
begin
Inc(n);
k:=0;
end
else
begin
n:=0;
k:=0;
end;
if k = C_CP then
begin
Result:=1;
for k:=j downto j-C_CP+1 do
begin PaintKN(knKrWin,k,i) end;
Exit;
end;
if n = C_CP then
begin
Result:=2;
for n:=j downto j-C_CP+1 do
begin PaintKN(knNolWin,n,i) end;
Exit;
end;
end;
end;
 // проверяем занятость всех клеток
A:=false;
for i:=0 to C_CC-1 do
begin
for j:=0 to C_CC-1 do
begin if P[i,j] = 0 then
begin A:=true; Break; end end;
if A then
begin Break end;
end;
if not A then
begin Result:=3; Exit; end;
Result:=0;
end;

function NewNol(var P:TPole):byte;
var x,y,i,j:byte; max:integer; valSum:TVal;
begin
if isYes(P)<>1 then
begin
max:=-1; x:=0; y:=0;
for i:=0 to C_CC-1 do
begin for j:=0 to C_CC-1 do
begin if P[i,j]=0 then
begin
valSum[i,j]:=Value(p,2,i,j)+Value(p,1,i,j);
if valSum[i,j]>max then
begin
x:=i;
y:=j;
max:=valSum[i,j]
end;
end end end;
if max>-1 then
begin
PaintKN(knNolik,x,y);
P[x,y]:=2;
end;
Result:=isYes(p);
end
else
begin Result:=1 end;
end;

function Value(P:TPole; a,x,y:byte):Integer;
var series,i,j,k:byte; sum,pow:Integer;
begin
p[x,y]:=a;
series:=0;
sum:=0;

for i:=0 to C_CP-1 do
begin
if (x+i-(C_CP-1))<0 then
begin Continue end;
if (x+i)>(C_CC-1) then
begin break end;
for j:=0 to C_CP-1 do
begin if ((p[x-(C_CP-1)+i+j,y]<>a) and (p[x-(C_CP-1)+i+j,y]<>0)) then
begin
series:=0;
Break
end
else
if (p[x-(C_CP-1)+i+j,y]=a) then
begin Inc(series) end end;
pow:=C_Val;
if series<>1 then
begin if series>C_CP then
begin
if a=2 then
begin pow:=10000 end
else
begin pow:=1000 end;
end
else
begin for k:=1 to series do
begin pow:=pow*C_Val end end end;
sum:=sum+pow;
series:=0;
end;

for i:=0 to C_CP-1 do
begin
if (y+i-(C_CP-1))<0 then
begin Continue end;
if (y+i)>(C_CC-1) then
begin Break end;
for j:=0 to C_CP-1 do
begin if ((p[x,y-(C_CP-1)+i+j]<>a) and (p[x,y-(C_CP-1)+i+j]<>0)) then
begin
series:=0;
Break
end
else
if (p[x,y-(C_CP-1)+i+j]=a) then
begin Inc(series) end end;
pow:=C_Val;
if series<>1 then
begin if series>C_CP then
begin
if a=2 then
begin pow:=10000 end
else
begin pow:=1000 end;
end
else
begin for k:=1 to series do
begin pow:=pow*C_Val end end end;
sum:=sum+pow;
series:=0;
end;

for i:=0 to C_CP-1 do
begin
if (y+i-(C_CP-1))<0 then
begin Continue end;
if (x+i-(C_CP-1))<0 then
begin Continue end;
if (x+i)>(C_CC-1) then
begin Break end;
if (y+i)>(C_CC-1) then
begin Break end;
for j:=0 to C_CP-1 do
begin if ((p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]<>a) and (p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]<>0)) then
begin
series:=0;
Break
end
else
if (p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]=a) then
begin Inc(series) end end;
pow:=2*C_Val;
if series<>1 then
begin if series>C_CP then
begin
if a=2 then
begin pow:=10000 end
else
begin pow:=1000 end;
end
else
begin for k:=1 to series do
begin pow:=pow*C_Val end end end;
sum:=sum+pow;
series:=0;
end;

for i:=0 to C_CP-1 do
begin
if (y+i-(C_CP-1))<0 then
begin Continue end;
if (x-i+(C_CP-1))>(C_CC-1) then
begin Continue end;
if (x-i)<0 then
begin Break end;
if (y+i)>(C_CC-1) then
begin Break end;
for j:=0 to C_CP-1 do
begin if ((p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]<>a) and (p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]<>0)) then
begin
series:=0;
Break
end
else
if (p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]=a) then
begin Inc(series) end end;
pow:=2*C_Val;
if series<>1 then
begin if series>C_CP then
begin
if a=2 then
begin pow:=10000 end
else
begin pow:=1000 end;
end
else
begin for k:=1 to series do
begin pow:=pow*C_Val end end end;
sum:=sum+pow;
series:=0;
end;
Result:=sum;
end;

procedure NewGame;
var i:integer;
begin
Form1.Image1.Picture:=nil;
Form1.Image1.Canvas.Pen.Color:=clBlack;
for i:=1 to C_CC-1 do
begin
Form1.Image1.Canvas.MoveTo(C_CellWH*i,0);
Form1.Image1.Canvas.LineTo(C_CellWH*i,Form1.Image1.Height);
Form1.Image1.Canvas.MoveTo(0,C_CellWH*i);
Form1.Image1.Canvas.LineTo(Form1.Image1.Width,C_CellWH*i);
end;
FillChar(Pl,SizeOf(pl),0);
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
Form1.ClientHeight:=C_CellWH*C_CC;
Form1.ClientWidth:=Form1.ClientHeight;    // C_CellWH*C_CC
Image1.ClientWidth:=Form1.ClientWidth;
Image1.ClientHeight:=Image1.ClientWidth;  //Form1.ClientHeight
Image1.Picture:=nil;
Image1.Canvas.Pen.Color:=clBlack;
for i:=1 to C_CC-1 do
begin
Image1.Canvas.MoveTo(C_CellWH*i,0);
Image1.Canvas.LineTo(C_CellWH*i,Image1.Height);
Image1.Canvas.MoveTo(0,C_CellWH*i);
Image1.Canvas.LineTo(Image1.Width,C_CellWH*i);
end;
FillChar(Pl,SizeOf(pl),0);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
var Mess:String;
begin
if Button in [mbRight,mbMiddle] then
begin Exit end;
if Pl[(X div C_CellWH), (Y div C_CellWH)] <> 0 then
begin Exit end;
PaintKN(knKrestik,(X div C_CellWH), (Y div C_CellWH));
Pl[(X div C_CellWH),(Y div C_CellWH)]:=1;
Case NewNol(Pl) of
0:
begin Mess:='' end;
1:
begin Mess:='Крестики выиграли!' end;
2:
begin Mess:='Нолики выиграли!' end;
3:
begin Mess:='Ничья' end;
End;
if Mess <> '' then
begin Case MessageDlg(Mess+#13#10+'Хотите сыграть ещё раз?',mtConfirmation,[mbYes, mbNo],0) of
mrYes:
begin NewGame end;
mrNo:
begin Close end;
End end;
end;
procedure TForm1.N2Click(Sender: TObject);
begin
NewGame;
end;
procedure TForm1.N3Click(Sender: TObject);
begin
Close;
end;
end.
