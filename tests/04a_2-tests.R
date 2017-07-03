#
# 04a_2 checks and tests
#



# check populations sizes ---------------------------------------------------
# against expected

# 'life time' risk in UK tb
n.uk_tb/(pop_year*0.3)

# 'life time' risk in UK tb of those 'detectable'
n.uk_tb/(pop_year*0.3*case_detection_rate)

# life time risk in UK and exit tb
(n.uk_tb + n.exit_tb)/(pop_year*0.3)

# life time risk in UK and exit tb of those 'detectable'
(n.uk_tb + n.exit_tb)/(pop_year*0.3*case_detection_rate)

# predicted number of active tb cases notified
pop_year*0.3*0.1*case_detection_rate
