function [title]=centre_title(titlestr_line1,titlestr_line2);
%CENTRE_TITLE Centres the strings in the title
%
%   [TITLE]=centre_title(TITLESTR_LINE1,TITLESTR_LINE2)

blanks = '                                                 ';
if (length(titlestr_line2) > 0)&&(length(titlestr_line2) >= length(titlestr_line1))
  %filler = blanks(1:int16((length(titlestr_line2)-length(titlestr_line1))/2));
  filler = blanks(1:(length(titlestr_line2)-length(titlestr_line1))/2);
  title = strvcat([filler titlestr_line1],[titlestr_line2]);
elseif (length(titlestr_line2) > 0)&&(length(titlestr_line2) < length(titlestr_line1))
  %filler = blanks(1:int16((length(titlestr_line1)-length(titlestr_line2))/2));
  filler = blanks(1:(length(titlestr_line1)-length(titlestr_line2))/2);
  title = strvcat([titlestr_line1],[filler titlestr_line2]);
else
  title = strvcat([titlestr_line1],'');
end