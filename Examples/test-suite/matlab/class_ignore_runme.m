a = class_ignore.Bar();

if (~strcmp(class_ignore.do_blah(a),'Bar::blah'))
  error('FAILED!!')
end
