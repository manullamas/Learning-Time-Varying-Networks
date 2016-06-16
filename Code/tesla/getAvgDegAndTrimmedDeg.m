function  d22 = getAvgDegAndTrimmedDeg(tt)

d22=0;
for i=1:size(tt,1);
    d22=d22 + length(find(abs(tt(i,:))>1e-2));
end
d22 = d22 / size(tt,1);