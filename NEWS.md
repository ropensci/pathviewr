# pathviewr 1.1.5
* Bug fixes to `plot_by_subject()` and other plotting functions to ensure 
compliance with ggplot2 v.3.4.0

# pathviewr 1.1.4
* `gather_tunnel_data()` now checks to see if mean marker error is present in
the data and skips such columns if they are absent

# pathviewr 1.1.3
* `get_2d_angle()` now accepts numeric vectors for each argument and will 
return a vector of angles of equal length

# pathviewr 1.1.2
* `viewr` attribute requirements have been largely relaxed. This is intended
to provide better flexibility of using functions at arbitrary points within the
"data analysis pipeline". 
* Users should be advised that since some safeguards have been removed, objects
passed through most `pathviewr` functions are not checked as thoroughly for all
formatting requirements.

# pathviewr 1.1.1
* Remove viewr attribute requirements. Work in progress

# pathviewr 1.1.0
* New data cleaning functions added: set_traj_frametime(),
get_traj_velocities(), clean_by_span(), remove_duplicate_frames(), and
remove_vel_anomalies()
* These new functions have not been thoroughly vetted nor have unit tests
been written for them -- please use with caution and report issues.

# pathviewr 1.0.0
* Package has been accepted by rOpenSci and is now hosted on ropensci/pathviewr
* No changes to code since v0.9.5

# pathviewr 0.9.5
* Package has been updated to incorporate feedback from rOpenSci reviewers
@asbonnetlebrun and @marcosci, along with editor @maelle. See here for more
details: ropensci/software-review#409

# pathviewr 0.9.4
* We are targeting a submission to rOpenSci in the near future (hopefully
today). This version of pathviewr has been prepped according to rOpenSci's
"Packages: Development, Maintenance, and Peer Review" guide.
