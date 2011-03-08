%define conf cloudbuilder.conf.example
%define rootdir %{_libdir}/%{name}
%define logdir %{_localstatedir}/log/%{name}

BuildRequires: erlang maven

Summary: Erlang Distribution (EDist) Agent
Name: agent
Version: ${project.rpmsafe.version}
License: GPL
Release: 1
BuildArch: noarch
Requires: erlang
Group: Systems Management
Source: agent-${project.version}-src.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-root

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
mkdir -p $RPM_BUILD_ROOT/%{_bindir}

# Install documentation  
%clean
mvn clean

%files
%defattr(-,root,root)
%{rootdir}

%changelog
