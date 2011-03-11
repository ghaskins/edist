%define rootdir %{_libdir}/%{name}
%define homedir %{_localstatedir}/lib/%{name}
%define logdir %{_localstatedir}/log/%{name}
%define realvsn ${project.version}

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

Requires: erlang facter sudo

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
install -d -m 755 $RPM_BUILD_ROOT/%{logdir} 
install -d -m 755 $RPM_BUILD_ROOT/%{homedir} 
mkdir -p $RPM_BUILD_ROOT/etc
mkdir -p $RPM_BUILD_ROOT/%{_sbindir}

install -m 600 %{SOURCE2} $RPM_BUILD_ROOT/%{homedir}/.erlang.cookie

cat > $RPM_BUILD_ROOT/etc/edist_agent.config <<EOF
[{edist_agent, [{rel, "client-release"}, {path, "%{homedir}"}]}].
EOF

cat > $RPM_BUILD_ROOT/%{_sbindir}/%{name} <<EOF 
#!/bin/bash

export ERL_LIBS=%{rootdir}/lib

run_erl -daemon %{homedir}/ %{logdir} "erl -smp -boot %{rootdir}/releases/%{realvsn}/start -sname edist_agentd -pidfile /var/run/edist_agent.pid -noshell -config /etc/edist_agent"
EOF

chmod a+x $RPM_BUILD_ROOT/%{_sbindir}/%{name}

#install init script
mkdir -p $RPM_BUILD_ROOT/etc/init.d
cat %{SOURCE1} | sed "s|%%DAEMON%%|%{_sbindir}/%{name}|" > $RPM_BUILD_ROOT/etc/init.d/%{name}

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
%dir %attr(0755, edist, root) %{homedir}
%attr(0600, edist, root) %{homedir}/.erlang.cookie
%dir %attr(0755, edist, root) %{logdir}
/etc/init.d/%{name}
/etc/edist_agent.config
%{_sbindir}/%{name}

%changelog
