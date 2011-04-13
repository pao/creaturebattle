function creaturebattle(home, away)
    if strcmp(home, away)
        disp(['The ' home ' refuses to fight itself!']);
    else
        salt = '.NaCl.$^d43lwz;)3s.optimize.this';
        mangled = mangle({home, away}, salt);
        mdhome = java.security.MessageDigest.getInstance('SHA1');
        mdaway = java.security.MessageDigest.getInstance('SHA1');
        mdhome.update(mangled(1).getBytes());
        mdaway.update(mangled(2).getBytes());
        battle = sortrows({[mod(double(mdhome.digest),255)' home]; [mod(double(mdaway.digest),255)' away]});
        disp(['The ' battle{1}(21:end) ' has won the battle!']);
    end
end

function mangled = mangle(monsters, salt)
    mangled = cellfun(@(x, y) java.lang.String([x y salt]), ...
        monsters, monsters(end:-1:1), 'UniformOutput', false);
end