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
  C_CellWH = 50; // ����� � ������ ����� ������
  C_CC = 10; // ���-�� ������ // = 10
  C_CP = 5; // ����� ���� �������� ������ ����, ������������ ��� ������
  C_Val = 5; //��������� �����������

type
  TKrestikNolik = (knKrestik,knKrWin,knNolik,knNolWin); // ������� ��� �����
  TPole = array [0..C_CC-1] of array [0..C_CC-1] of Byte;
  TVal = array [0..C_CC-1] of array [0..C_CC-1] of Integer; //�������� ��������� �������
                                                            //��� ������ ������ ����
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
  knKrestik: begin
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

  knKrWin: begin
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

  knNolik: begin
            Form1.Image1.Canvas.Pen.Color:=clBlue;
            Form1.Image1.Canvas.Brush.Style:=bsClear;
            Form1.Image1.Canvas.Ellipse( (C_CellWH div 8) + CellX*C_CellWH,
                                         (C_CellWH div 8) + CellY*C_CellWH,
                                          7*(C_CellWH div 8) + CellX*C_CellWH,
                                          7*(C_CellWH div 8) + CellY*C_CellWH  );
           end;

  knNolWin: begin
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
 // ��������� ���������
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
   else if (p[i-x,i]=2) then
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
   for k:=i downto i-C_CP+1 do PaintKN(knKrWin,k-x,k);
   Exit;
  end;
 if n = C_CP then
  begin
   Result:=2;
   for n:=i downto i-C_CP+1 do PaintKN(knNolWin,n-x,n);
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
   else if (p[i,i-x]=2) then
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
   for k:=i downto i-C_CP+1 do PaintKN(knKrWin,k,k-x);
   Exit;
  end;
 if n = C_CP then
  begin
   Result:=2;
   for n:=i downto i-C_CP+1 do PaintKN(knNolWin,n,n-x);
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
   else if (p[i-x,C_CC-1-i]=2) then
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
   for k:=i downto i-C_CP+1 do PaintKN(knKrWin,k-x,C_CC-1-k);
   Exit;
  end;
 if n = C_CP then
  begin
   Result:=2;
   for n:=i downto i-C_CP+1 do PaintKN(knNolWin,n-x,C_CC-1-n);
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
   else if (p[i,C_CC-1-i+x]=2) then
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
   for k:=i downto i-C_CP+1 do PaintKN(knKrWin,k,C_CC-1-k+x);
   Exit;
  end;
 if n = C_CP then
  begin
   Result:=2;
   for n:=i downto i-C_CP+1 do PaintKN(knNolWin,n,C_CC-1-n+x);
   Exit;
  end;
 end;
 end;
 for i:=0 to C_CC-1 do
  begin
   // ���������
   k:=0; n:=0;
   for j:=0 to C_CC-1 do
   begin
   if (p[i,j]=1) then
     begin
       Inc(k);
       n:=0;
     end
   else if (p[i,j]=2) then
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
     for k:=j downto j-C_CP+1 do PaintKN(knKrWin,i,k);
     Exit;
    end;
   if n = C_CP then
    begin
     Result:=2;
     for n:=j downto j-C_CP+1 do PaintKN(knNolWin,i,n);
     Exit;
    end;
   end;
   // �����������
   k:=0; n:=0;
   for j:=0 to C_CC-1 do
   begin
   if p[j,i] = 1 then
     begin
       Inc(k);
       n:=0;
     end
   else if (p[j,i]=2) then
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
      for k:=j downto j-C_CP+1 do PaintKN(knKrWin,k,i);
      Exit;
     end;
   if n = C_CP then
     begin
      Result:=2;
      for n:=j downto j-C_CP+1 do PaintKN(knNolWin,n,i);
      Exit;
     end;
   end;
  end;
 // ��������� ��������� ���� ������
 A:=false;
 for i:=0 to C_CC-1 do
  begin
   for j:=0 to C_CC-1 do
   if P[i,j] = 0 then begin A:=true; Break; end;
   if A then Break;
  end;
 if not A then begin Result:=3; Exit; end;
 Result:=0;
end;

function NewNol(var P:TPole):byte;
var x,y,i,j:byte; max:integer; valSum:TVal;
begin
 if isYes(P)<>1 then
 begin
   max:=-1; x:=0; y:=0;
   for i:=0 to C_CC-1 do
    for j:=0 to C_CC-1 do
     if P[i,j]=0 then
      begin
       valSum[i,j]:=Value(p,2,i,j)+Value(p,1,i,j);
       if valSum[i,j]>max then
        begin
          x:=i;
          y:=j;
          max:=valSum[i,j]
        end;
      end;
   if max>-1 then
    begin
     PaintKN(knNolik,x,y);
     P[x,y]:=2;
    end;
   Result:=isYes(p);
 end
  else Result:=1;
end;

function Value(P:TPole; a,x,y:byte):Integer;
var series,i,j,k:byte; sum,pow:Integer;
begin
 p[x,y]:=a;
 series:=0;
 sum:=0;

 for i:=0 to C_CP-1 do
  begin
   if (x+i-(C_CP-1))<0 then Continue;
   if (x+i)>(C_CC-1) then break;
   for j:=0 to C_CP-1 do
    if ((p[x-(C_CP-1)+i+j,y]<>a) and (p[x-(C_CP-1)+i+j,y]<>0)) then
     begin
      series:=0;
      Break
     end
     else if (p[x-(C_CP-1)+i+j,y]=a) then Inc(series);
  pow:=C_Val;
  if series<>1 then
   if series>C_CP then
    begin
     if a=2 then pow:=10000
       else pow:=1000;
    end
    else for k:=1 to series do pow:=pow*C_Val;
  sum:=sum+pow;
  series:=0;
  end;

 for i:=0 to C_CP-1 do
  begin
   if (y+i-(C_CP-1))<0 then Continue;
   if (y+i)>(C_CC-1) then Break;
   for j:=0 to C_CP-1 do
    if ((p[x,y-(C_CP-1)+i+j]<>a) and (p[x,y-(C_CP-1)+i+j]<>0)) then
     begin
      series:=0;
      Break
     end
     else if (p[x,y-(C_CP-1)+i+j]=a) then Inc(series);
  pow:=C_Val;
  if series<>1 then
   if series>C_CP then
    begin
     if a=2 then pow:=10000
       else pow:=1000;
    end
    else for k:=1 to series do pow:=pow*C_Val;
  sum:=sum+pow;
  series:=0;
  end;

 for i:=0 to C_CP-1 do
  begin
   if (y+i-(C_CP-1))<0 then Continue;
   if (x+i-(C_CP-1))<0 then Continue;
   if (x+i)>(C_CC-1) then Break;
   if (y+i)>(C_CC-1) then Break;
   for j:=0 to C_CP-1 do
    if ((p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]<>a) and (p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]<>0)) then
     begin
      series:=0;
      Break
     end
     else if (p[x-(C_CP-1)+i+j,y-(C_CP-1)+i+j]=a) then Inc(series);
  pow:=2*C_Val;
  if series<>1 then
   if series>C_CP then
    begin
     if a=2 then pow:=10000
       else pow:=1000;
    end
    else for k:=1 to series do pow:=pow*C_Val;
  sum:=sum+pow;
  series:=0;
  end;

 for i:=0 to C_CP-1 do
  begin
   if (y+i-(C_CP-1))<0 then Continue;
   if (x-i+(C_CP-1))>(C_CC-1) then Continue;
   if (x-i)<0 then Break;
   if (y+i)>(C_CC-1) then Break;
   for j:=0 to C_CP-1 do
    if ((p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]<>a) and (p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]<>0)) then
     begin
      series:=0;
      Break
     end
     else if (p[x+(C_CP-1)-i-j,y-(C_CP-1)+i+j]=a) then Inc(series);
  pow:=2*C_Val;
  if series<>1 then
   if series>C_CP then
    begin
     if a=2 then pow:=10000
       else pow:=1000;
    end
    else for k:=1 to series do pow:=pow*C_Val;
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
 if Button in [mbRight,mbMiddle] then Exit;
 if Pl[(X div C_CellWH), (Y div C_CellWH)] <> 0 then Exit;
 PaintKN(knKrestik,(X div C_CellWH), (Y div C_CellWH));
 Pl[(X div C_CellWH),(Y div C_CellWH)]:=1;
 Case NewNol(Pl) of
  0: Mess:='';
  1: Mess:='�������� ��������!';
  2: Mess:='������ ��������!';
  3: Mess:='�����';
 End;
 if Mess <> '' then
  Case MessageDlg(Mess+#13#10+'������ ������� ��� ���?',mtConfirmation,[mbYes, mbNo],0) of
   mrYes: NewGame;
   mrNo: Close;
  End;
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
