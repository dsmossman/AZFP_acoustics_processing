function Sv_R = TS_to_Sv(TS_min,R,freq)
    %% Uses the Rudstam et al., 2009 equation 1 to determine a range-varying Sv threshold from a given TS_min value
    
    freqlist = [38,125,200];
    
    c = 1450.50;
    taulist = [300,200,250];
    sigmalist = [0.1306,0.0483,0.0094];
    
    index=find(freqlist == freq);
    tau = taulist(index) * 10^-6; % conversion from microseconds to seconds
    sigma = sigmalist(index);

    Sv_R = -(20 * log(R) + 10 * log((c * tau * sigma)/2) - TS_min + 6);

end