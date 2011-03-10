%define rootdir %{_libdir}/%{name}
%define homedir %{_localstatedir}/lib/%{name}
%define logdir %{_localstatedir}/log/%{name}

BuildRequires: erlang maven

Summary: Erlang Distribution (EDist) Agent
Name: edist_agent
Version: ${project.rpmsafe.version}
License: GPL
Release: 1
BuildArch: noarch
Group: Systems Management
Source: agent-${project.version}-src.tar.gz
Source1: edist_agent.init
Source2: default-cookie
BuildRoot: %{_tmppath}/%{name}-%{version}-root

Requires: erlang facter

%if 0%{?suse_version} > 1110  
Requires(pre):    pwdutils  
%else  
Requires(pre):    shadow-utils
%endif

%description
Authors
--------------------------
  Gregory Haskins <ghaskins@novell.com>

%debug_package
%prep
%setup -c

%build

%install
mvn install -DoutputDirectory=$RPM_BUILD_ROOT%{rootdir}
mkdir -p $RPM_BUILD_ROOT/%{logdir}
mkdir -p $RPM_BUILD_ROOT/%{homedir}
mkdir -p $RPM_BUILD_ROOT/etc

install -m 600 %{SOURCE2} $RPM_BUILD_ROOT/%{homedir}/.erlang.cookie

cat > $RPM_BUILD_ROOT/etc/edist_agent.config <<EOF
[{edist_agent, [{rel, "client-release"}, {path, "%{homedir}"}]}].
EOF

#install init script
mkdir -p $RPM_BUILD_ROOT/etc/init.d
cat %{SOURCE1} | sed "s|%FINAL_ROOTDIR%|%{rootdir}|" | sed "s|%FINAL_HOME%|%{homedir}|" | sed "s|%FINAL_LOGDIR%|%{logdir}|" > $RPM_BUILD_ROOT/etc/init.d/%{name}

chmod a+x $RPM_BUILD_ROOT/etc/init.d/%{name}

%clean
mvn clean

%pre
getent group edist >/dev/null || groupadd -r edist
getent passwd edist >/dev/null || \
useradd -r -g edist -d %{homedir} -s /sbin/nologin \
-c "Erlang Distribution Agent daemon" edist
exit 0

%post
%fillup_and_insserv -f -y %{name}

%preun
%stop_on_removal %{name}

%postun
%insserv_cleanup

%files
%defattr(-,root,root)
%{rootdir}
%{homedir}
%{logdir}
/etc/init.d/%{name}
/etc/edist_agent.config

%changelog
