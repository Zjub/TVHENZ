# Sourced by 03_make_graphs.R, sharing its theme and export functions.
s <- read('special_vote_hurdles')
d <- s |> filter(leader=='National') |> slice_min(net_margin_needed_pp_central,n=6) |>
 arrange(net_margin_needed_pp_central) |>
 mutate(display=sprintf('%s\nNational +%s; benchmark ~%s specials',electorate,scales::comma(absolute_margin),scales::comma(round(specials_benchmark/100)*100)),
 display=factor(display,levels=rev(display)))
p<-ggplot(d,aes(net_margin_needed_pp_central,display))+
 geom_segment(aes(x=net_margin_needed_pp_more,xend=net_margin_needed_pp_fewer,yend=display),linewidth=1.2,colour='#A6BBC2')+
 geom_point(size=4,colour='#007C91')+
 geom_text(aes(label=sprintf('%.1f pp',net_margin_needed_pp_central)),vjust=-1,size=4.5,colour='#142D40')+
 scale_x_continuous(breaks=seq(0,80,10),limits=c(0,80),expand=expansion(mult=c(.02,.02)))+
 labs(subtitle='Break-even Labour minus National gap among all valid special candidate votes\nVolume benchmark scales the national estimate using each electorate\'s 2023 specials pattern')
save('P20',p,d,'Which National leads are smallest relative to specials?','Labour minus National special-vote share needed to tie (percentage points)','Electorate and preliminary National lead',
 'Source: Electoral Commission 2026 preliminary counts and 2023 candidate-vote turnout table; own scenarios.\nDots: historical-pattern benchmark. Lines: 0.5 to 1.5 times that volume, not confidence intervals.\nSix lowest hurdles among nine smallest National leads. No boundary/rule adjustment; recount changes excluded.','Main deck')
d<-read('special_vote_volume_curves') |> filter(electorate %in% c('Wairarapa','Hamilton East','Whangarei','Nelson','Takanini'))
cols<-c('Wairarapa'='#007C91','Hamilton East'='#C28000','Whangarei'='#7955A1','Nelson'='#AD4A3F','Takanini'='#495C32')
p<-ggplot(d,aes(special_valid_candidate_votes,net_labour_lead_needed_pp,colour=electorate))+
 geom_line(linewidth=1.2)+scale_colour_manual(values=cols)+
 scale_x_continuous(breaks=seq(2000,10000,2000),labels=scales::comma)+
 labs(subtitle='A larger pool of specials needs a smaller Labour lead to erase the same preliminary margin')
save('P21',p,d,'How close National contests depend on special-vote volume','Valid special candidate votes remaining in that electorate (scenario)','Labour minus National share needed to tie (percentage points)',
 'Source: Electoral Commission preliminary candidate margins; own break-even calculation: 100 x margin / valid specials.\nEach curve uses a range of possible volumes, not an official local count or a probability forecast.\nVotes for other candidates do not transfer to Labour or National in NZ electorate voting.')
d<-s |> filter(absolute_margin<=2200) |> arrange(net_margin_needed_pp_central) |>
 mutate(display=sprintf('%s  |  %s +%s',electorate,leader,scales::comma(absolute_margin)),display=factor(display,levels=rev(display)),
 chase=if_else(leader=='National','Labour chasing National','National chasing Labour'))
p<-ggplot(d,aes(net_margin_needed_pp_central,display,colour=chase))+
 geom_segment(aes(x=0,xend=net_margin_needed_pp_central,yend=display),linewidth=1,colour='#B9C8CE')+
 geom_point(size=4)+geom_text(aes(label=sprintf('%.1f pp',net_margin_needed_pp_central)),hjust=-.3,size=4.5)+
 scale_colour_manual(values=c('Labour chasing National'='#007C91','National chasing Labour'='#AD4A3F'))+
 scale_x_continuous(expand=expansion(mult=c(.02,.18)),breaks=seq(0,60,10))+
 labs(subtitle='Net National losses matter for the House: gains in Labour-held seats would offset losses elsewhere')
save('P22',p,d,'National could also gain seats on special votes','Trailing candidate minus leader special-vote share needed to tie (percentage points)','Electorate, current leader and preliminary margin',
 'Source: Electoral Commission preliminary counts and 2023 turnout table; own historical-pattern volume benchmark.\nNational-Labour marginals within 2,200 votes. These are hurdles, not forecasts; smaller does not mean more likely.\nUnadjusted predecessor electorates: Kapiti uses Otaki; Hutt South boundaries changed. Final party votes also matter.')
d<-s |> filter(leader=='National') |> select(electorate,net_margin_needed_pp_central,net_margin_needed_pp_uniform) |>
 pivot_longer(-electorate,names_to='method',values_to='gap') |>
 mutate(method=recode(method,net_margin_needed_pp_central='2023 local pattern',net_margin_needed_pp_uniform='Uniform national ratio'),
 electorate=factor(electorate,levels=rev(c('Wairarapa','Hamilton East','Whangarei','Nelson','Takanini','Kapiti','Hamilton West','Maungakiekie','Northcote'))))
p<-ggplot(d,aes(gap,electorate,colour=method))+geom_point(position=position_dodge(width=.45),size=4)+
 scale_colour_manual(values=c('2023 local pattern'='#007C91','Uniform national ratio'='#C28000'))+
 labs(subtitle='A high-special-vote electorate can be more vulnerable than its raw lead suggests')
save('P23',p,d,'The ranking depends on how many specials each seat has','Labour minus National special-vote share needed to tie (percentage points)','Electorate',
 'Source: Electoral Commission preliminary counts and official 2023 valid-candidate-vote turnout table; own benchmarks.\nBoth methods use the same national 2026 specials estimate. Neither is an official electorate-level estimate or a forecast.\nHistorical relative volumes may change with boundaries, enrolment rules, overseas voting and candidate-vote eligibility.')
