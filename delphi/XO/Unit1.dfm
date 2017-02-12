object Form1: TForm1
  Left = 218
  Top = 129
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1050#1088#1077#1089#1090#1080#1082#1080'-'#1085#1086#1083#1080#1082#1080
  ClientHeight = 302
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 308
    Height = 308
    OnMouseUp = FormMouseUp
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object N1: TMenuItem
      Caption = #1052#1077#1085#1102
      object N2: TMenuItem
        Caption = #1053#1086#1074#1072#1103' '#1080#1075#1088#1072
        OnClick = N2Click
      end
      object N3: TMenuItem
        Caption = #1042#1099#1093#1086#1076
        OnClick = N3Click
      end
    end
  end
end
